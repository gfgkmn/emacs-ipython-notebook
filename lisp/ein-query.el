;;; ein-query.el --- jQuery like interface on top of curl -*- lexical-binding: t -*-

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-query.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-query.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-query.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'request)
(require 'url)
(require 'ein-core)
(require 'ein-log)

;; Forward declarations for functions used in re-auth flow
(declare-function ein:jupyter-crib-token "ein-jupyter")
(declare-function ein:notebooklist-login--iteration "ein-notebooklist")

(defcustom ein:query-timeout 10000
  "Default query timeout for HTTP access in millisecond."
  :type '(choice (integer :tag "Timeout [ms]" 1000)
                 (const :tag "No timeout" nil))
  :group 'ein)

(defvar ein:query-xsrf-cache (make-hash-table :test 'equal)
  "Remember the last xsrf token by host.
This is a hack in case we catch cookie jar in transition.
The proper fix is to sempahore between competing curl processes.")

(defvar ein:query-authorization-tokens (make-hash-table :test 'equal)
  "Jupyterhub authorization token by (host . username).")

(defvar ein:query-reauth-in-progress nil
  "Non-nil when re-authentication is in progress to prevent recursion.")

(defvar ein:query-reauth-max-retries 1
  "Maximum re-auth attempts per request before giving up.")

(defun ein:query-get-cookies (host path-prefix)
  "Return (:path :expire :name :value) for HOST, matching PATH-PREFIX."
  (when-let ((filename (request--curl-cookie-jar)))
    (with-temp-buffer
      (insert-file-contents filename)
      (cl-loop for (domain _flag path _secure _http-only expire name value)
               in (request--netscape-cookie-parse)
               when (and (string= domain host)
                         (cl-search path-prefix path))
               collect `(:path ,path :expire ,expire :name ,name :value ,value)))))

(defun ein:query-prepare-header (url settings &optional securep)
  "Ensure that REST calls to the jupyter server have the correct _xsrf argument."
  (let* ((host (url-host (url-generic-parse-url url)))
         (paths* (let* ((warning-minimum-level :emergency)
                        (warning-minimum-log-level :emergency)
                        (root-url (car (ein:notebooklist-parse-nbpath url))))
                   (if root-url
                       (let ((root-path (url-filename (url-generic-parse-url root-url))))
                         (unless (zerop (length root-path))
                           (list (file-name-as-directory root-path))))
                     (let* ((url* url)
                            (parsed-url* (url-generic-parse-url url*))
                            paths*)
                       (while (not (zerop (length (url-filename parsed-url*))))
                         (push (file-name-as-directory (url-filename parsed-url*)) paths*)
                         (setq url* (file-name-directory (directory-file-name url*))
                               parsed-url* (url-generic-parse-url url*)))
                       paths*))))
         (paths (progn (cl-pushnew "/" paths* :test #'equal) (reverse paths*)))
         (cookies (cl-some (lambda (path)
                             (request-cookie-alist host path securep))
                           paths))
         (xsrf (or (cdr (assoc-string "_xsrf" cookies))
                   (gethash host ein:query-xsrf-cache)))
         (key (ein:query-divine-authorization-tokens-key url))
         (token (aand key
                      (gethash key ein:query-authorization-tokens)
                      (cons "Authorization" (format "token %s" it)))))
    (setq settings (plist-put settings :headers
                              (append (plist-get settings :headers)
                                      (list (cons "User-Agent" "Mozilla/5.0")))))
    (when token
      (setq settings (plist-put settings :headers
                                (append (plist-get settings :headers)
                                        (list token)))))
    (when xsrf
      (setq settings (plist-put settings :headers
                                (append (plist-get settings :headers)
                                        (list (cons "X-XSRFTOKEN" xsrf)))))
      (setf (gethash host ein:query-xsrf-cache) xsrf))
    (setq settings (plist-put settings :encoding 'binary))
    settings))

(defun ein:query-divine-authorization-tokens-key (url)
  "Infer semblance of jupyterhub root.
From https://hub.data8x.berkeley.edu/hub/user/806b3e7/notebooks/Untitled.ipynb,
get (\"hub.data8x.berkeley.edu\" . \"806b3e7\")"
  (-when-let* ((parsed-url (url-generic-parse-url url))
               (url-host (url-host parsed-url))
               (slash-path (car (url-path-and-query parsed-url)))
               (components (split-string slash-path "/" t)))
    (awhen (member "user" components)
      (cons url-host (cl-second it)))))

(defun ein:query-url-no-api (url)
  "Extract base URL-OR-PORT from full API URL.
E.g., `http://localhost:8888/api/sessions' -> `http://localhost:8888'."
  (let* ((parsed (url-generic-parse-url url))
         (port (url-port parsed)))
    (if port
        (format "%s://%s:%s" (url-type parsed) (url-host parsed) port)
      (format "%s://%s" (url-type parsed) (url-host parsed)))))

(defun ein:query-clear-auth-cache (url-or-port)
  "Clear cached authentication credentials for URL-OR-PORT."
  (let* ((parsed (url-generic-parse-url url-or-port))
         (host (url-host parsed)))
    (ein:log 'info "Clearing auth cache for %s" url-or-port)
    (remhash host ein:query-xsrf-cache)
    (when-let ((key (ein:query-divine-authorization-tokens-key url-or-port)))
      (remhash key ein:query-authorization-tokens))))

(defun ein:query--wrap-error-for-reauth (url settings original-error)
  "Wrap error handler to intercept 403 and trigger re-auth.
URL is the request URL. SETTINGS is the request settings plist.
ORIGINAL-ERROR is the original error callback to call if not handling 403."
  (cl-function
   (lambda (&key response error-thrown &allow-other-keys)
     (let ((status (request-response-status-code response))
           (retry-count (or (plist-get settings :ein--reauth-retry) 0)))
       (if (and (eq status 403)
                (not ein:query-reauth-in-progress)
                (< retry-count ein:query-reauth-max-retries))
           ;; Trigger re-auth flow
           (ein:query--handle-403 url settings original-error response error-thrown)
         ;; Call original error handler
         (when original-error
           (funcall original-error :response response :error-thrown error-thrown)))))))

(defun ein:query--handle-403 (url settings original-error response error-thrown)
  "Handle 403 by attempting silent re-auth, then prompting if needed.
URL is the failed request URL. SETTINGS is the request settings plist.
ORIGINAL-ERROR is the original error callback. RESPONSE and ERROR-THROWN
are from the failed request."
  (let* ((url-or-port (ein:query-url-no-api url))
         (retry-count (or (plist-get settings :ein--reauth-retry) 0)))
    (ein:query-clear-auth-cache url-or-port)
    (setq ein:query-reauth-in-progress t)
    (ein:log 'info "Session expired for %s, attempting silent re-auth" url-or-port)
    ;; Try to get token from running Jupyter server (without prompting)
    (cl-multiple-value-bind (password-p token) (ein:jupyter-crib-token url-or-port)
      (if (and (stringp token) (eq password-p :json-false))
          ;; Have token - attempt silent re-auth
          (ein:notebooklist-login--iteration
           url-or-port
           ;; Success callback - replay request
           (lambda (_buffer _url)
             (setq ein:query-reauth-in-progress nil)
             (ein:log 'info "Silent re-auth successful, retrying request")
             (let ((new-settings (plist-put (copy-sequence settings)
                                            :ein--reauth-retry (1+ retry-count))))
               (apply #'ein:query-singleton-ajax url new-settings)))
           ;; Error callback - prompt user
           (lambda ()
             (ein:query--prompt-reauth url settings original-error response error-thrown))
           token 0 nil)
        ;; No token available or password required - prompt user
        (ein:query--prompt-reauth url settings original-error response error-thrown)))))

(defun ein:query--prompt-reauth (url settings original-error response error-thrown)
  "Prompt user for password after silent re-auth fails.
URL is the failed request URL. SETTINGS is the request settings plist.
ORIGINAL-ERROR is the original error callback. RESPONSE and ERROR-THROWN
are from the failed request."
  (setq ein:query-reauth-in-progress nil)
  (let* ((url-or-port (ein:query-url-no-api url))
         (retry-count (or (plist-get settings :ein--reauth-retry) 0)))
    (if (y-or-n-p (format "Session expired for %s. Enter password? " url-or-port))
        (let ((password (read-passwd (format "Password for %s: " url-or-port))))
          (setq ein:query-reauth-in-progress t)
          (ein:notebooklist-login--iteration
           url-or-port
           (lambda (_buffer _url)
             (setq ein:query-reauth-in-progress nil)
             (ein:log 'info "Re-auth successful, retrying request")
             (let ((new-settings (plist-put (copy-sequence settings)
                                            :ein--reauth-retry (1+ retry-count))))
               (apply #'ein:query-singleton-ajax url new-settings)))
           (lambda ()
             (setq ein:query-reauth-in-progress nil)
             (ein:log 'error "Re-auth failed for %s" url-or-port)
             (when original-error
               (funcall original-error :response response :error-thrown error-thrown)))
           password 0 nil))
      ;; User declined
      (ein:log 'warn "User declined re-authentication for %s" url-or-port)
      (when original-error
        (funcall original-error :response response :error-thrown error-thrown)))))

(cl-defun ein:query-singleton-ajax (url &rest settings
                                        &key (timeout ein:query-timeout)
                                        &allow-other-keys)
  (if (executable-find request-curl)
      (let* ((request-backend 'curl)
             ;; Wrap error handler for 403 interception and re-auth
             (original-error (plist-get settings :error))
             (wrapped-error (ein:query--wrap-error-for-reauth url settings original-error)))
        (setq settings (plist-put settings :error wrapped-error))
        (when timeout
          (setq settings (plist-put settings :timeout (/ timeout 1000.0))))
        (unless (plist-member settings :sync)
          (setq settings (plist-put settings :sync ein:force-sync)))
        (apply #'request (url-encode-url url) (ein:query-prepare-header url settings)))
    (ein:display-warning
     (format "The %s program was not found, aborting" request-curl)
     :error)))

(defun ein:query-kernelspecs (url-or-port callback &optional iteration)
  "Send for kernelspecs of URL-OR-PORT with CALLBACK arity 0 (just a semaphore)"
  (setq iteration (or iteration 0))
  (ein:query-singleton-ajax
   (ein:url url-or-port "api/kernelspecs")
   :type "GET"
   :parser #'ein:json-read
   :complete (apply-partially #'ein:query-kernelspecs--complete url-or-port)
   :success (apply-partially #'ein:query-kernelspecs--success url-or-port callback)
   :error (apply-partially #'ein:query-kernelspecs--error url-or-port callback iteration)))

(defun ein:normalize-kernelspec-language (name)
  "Normalize the kernelspec language string"
  (if (stringp name)
      (replace-regexp-in-string "[ ]" "-" name)
    name))

(cl-defun ein:query-kernelspecs--success (url-or-port callback
                                          &key data _symbol-status _response
                                          &allow-other-keys)
  (let ((ks (list :default (plist-get data :default)))
        (specs (ein:plist-iter (plist-get data :kernelspecs))))
    (setf (gethash url-or-port *ein:kernelspecs*)
          (ein:flatten (dolist (spec specs ks)
                         (let ((name (car spec))
                               (info (cdr spec)))
                           (push (list name (make-ein:$kernelspec :name (plist-get info :name)
                                                                  :display-name (plist-get (plist-get info :spec)
                                                                                           :display_name)
                                                                  :resources (plist-get info :resources)
                                                                  :language (ein:normalize-kernelspec-language
                                                                             (plist-get (plist-get info :spec)
                                                                                        :language))
                                                                  :spec (plist-get info :spec)))
                                 ks))))))
  (when callback (funcall callback)))

(cl-defun ein:query-kernelspecs--error
    (url-or-port callback iteration
     &key data response error-thrown &allow-other-keys
     &aux
     (response-status (request-response-status-code response))
     (hub-p (request-response-header response "x-jupyterhub-version")))
  (if (< iteration 3)
      (if (and hub-p (eq response-status 405))
          (ein:query-kernelspecs--success url-or-port callback :data data)
        (ein:log 'verbose "Retry kernelspecs #%s in response to %s"
                 iteration response-status)
        (ein:query-kernelspecs url-or-port callback (1+ iteration)))
    (ein:log 'error
      "ein:query-kernelspecs--error %s: ERROR %s DATA %s"
      url-or-port (car error-thrown) (cdr error-thrown))
    (when callback (funcall callback))))

(cl-defun ein:query-kernelspecs--complete (_url-or-port &key data response &allow-other-keys
                                           &aux (resp-string (format "STATUS: %s DATA: %s" (request-response-status-code response) data)))
  (ein:log 'debug "ein:query-kernelspecs--complete %s" resp-string))

(defun ein:query-notebook-api-version (url-or-port callback)
  "Get notebook version of URL-OR-PORT with CALLBACK arity 0 (a semaphore)."
  (ein:query-singleton-ajax
   (ein:url url-or-port "api/spec.yaml")
   ;; the melpa yaml package was taking too long, unfortunately
   :parser (lambda ()
	     (if (re-search-forward "api\\s-+version: \\(\\S-+\\)"
				    nil t)
		 ;; emacs-25.3 doesn't have the right string-trim
		 (string-remove-prefix
		  "\"" (string-remove-suffix
			"\"" (match-string-no-properties 1)))
	       ""))
   :complete (apply-partially #'ein:query-notebook-api-version--complete
                              url-or-port callback)))

(cl-defun ein:query-notebook-api-version--complete
    (url-or-port callback
     &key data response
     &allow-other-keys &aux
     (resp-string (format "STATUS: %s DATA: %s"
                          (request-response-status-code response) data))
     (hub-p (request-response-header response "x-jupyterhub-version")))
  (ein:log 'debug "ein:query-notebook-api-version--complete %s" resp-string)
  (if (not (zerop (string-to-number data)))
      (setf (gethash url-or-port *ein:notebook-api-version*) data)
    (if hub-p
        (let ((key (ein:query-divine-authorization-tokens-key url-or-port)))
          (remhash key ein:query-authorization-tokens)
          (ein:display-warning
           (format "Server for user %s requires start, aborting"
                   (or (cdr key) "unknown"))
           :error)
          (setq callback nil))
      (ein:log 'warn "notebook api version currently unknowable")))
  (when callback (funcall callback)))

(provide 'ein-query)

;;; ein-query.el ends here
