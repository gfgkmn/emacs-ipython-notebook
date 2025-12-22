;;; ein-cell-history.el --- Undo history for cell content -*- lexical-binding:t -*-

;; Copyright (C) 2024 gfgkmn

;; Author: gfgkmn <gfgkmn@gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-cell-history.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-cell-history.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-cell-history.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Per-cell undo/redo history for EIN notebooks.
;; Captures cell state before execution and before edit-cell-done.
;; Each cell maintains its own history ring (up to ein:cell-history-max-length entries).

;;; Code:

(require 'ein-cell)

;;; Data structures

(defvar ein:cell-history-table (make-hash-table :test 'equal)
  "Hash table: cell-id -> list of previous text states (newest first).")

(defvar ein:cell-redo-table (make-hash-table :test 'equal)
  "Hash table: cell-id -> list of redo states.")

(defvar ein:cell-last-snapshot-table (make-hash-table :test 'equal)
  "Hash table: cell-id -> last snapshotted text (to detect changes).")

(defcustom ein:cell-history-max-length 10
  "Maximum number of history entries per cell."
  :type 'integer
  :group 'ein)

;;; Core functions

(defun ein:cell-history-push (cell)
  "Push current CELL text onto history ring.
Clears redo stack and updates snapshot."
  (let* ((cell-id (slot-value cell 'cell-id))
         (current-text (ein:cell-get-text cell))
         (history (gethash cell-id ein:cell-history-table)))
    ;; Push current text to history
    (push current-text history)
    ;; Trim to max length
    (when (> (length history) ein:cell-history-max-length)
      (setq history (seq-take history ein:cell-history-max-length)))
    (puthash cell-id history ein:cell-history-table)
    ;; Clear redo stack on new edit
    (remhash cell-id ein:cell-redo-table)
    ;; Update snapshot
    (puthash cell-id current-text ein:cell-last-snapshot-table)))

(defun ein:cell-maybe-save-history (cell)
  "Save history if CELL content changed since last snapshot.
Call this before executing a cell to capture pre-execution state."
  (let* ((cell-id (slot-value cell 'cell-id))
         (current-text (ein:cell-get-text cell))
         (last-snapshot (gethash cell-id ein:cell-last-snapshot-table)))
    (cond
     ;; First time seeing this cell - just record snapshot, don't push
     ((null last-snapshot)
      (puthash cell-id current-text ein:cell-last-snapshot-table))
     ;; Content changed - push old state to history
     ((not (string= current-text last-snapshot))
      (let ((history (gethash cell-id ein:cell-history-table)))
        (push last-snapshot history)
        (when (> (length history) ein:cell-history-max-length)
          (setq history (seq-take history ein:cell-history-max-length)))
        (puthash cell-id history ein:cell-history-table)
        ;; Clear redo stack on new edit
        (remhash cell-id ein:cell-redo-table)
        ;; Update snapshot to current
        (puthash cell-id current-text ein:cell-last-snapshot-table))))))

;;; Interactive commands

(defun ein:cell-undo ()
  "Undo current cell to previous edit state.
Pushes current state to redo stack before restoring."
  (interactive)
  (if-let* ((cell (ein:worksheet-get-current-cell :noerror t))
            (cell-id (slot-value cell 'cell-id))
            (history (gethash cell-id ein:cell-history-table))
            (prev-text (car history)))
      (progn
        ;; Push current to redo
        (let ((current-text (ein:cell-get-text cell))
              (redo (gethash cell-id ein:cell-redo-table)))
          (push current-text redo)
          (puthash cell-id redo ein:cell-redo-table))
        ;; Pop from history and apply
        (puthash cell-id (cdr history) ein:cell-history-table)
        (ein:cell-set-text cell prev-text)
        (puthash cell-id prev-text ein:cell-last-snapshot-table)
        (message "Cell undone (%d more in history)" (length (cdr history))))
    (message "No cell history to undo")))

(defun ein:cell-redo ()
  "Redo previously undone cell edit.
Pushes current state to history before restoring."
  (interactive)
  (if-let* ((cell (ein:worksheet-get-current-cell :noerror t))
            (cell-id (slot-value cell 'cell-id))
            (redo-stack (gethash cell-id ein:cell-redo-table))
            (next-text (car redo-stack)))
      (progn
        ;; Push current to history
        (let ((current-text (ein:cell-get-text cell))
              (history (gethash cell-id ein:cell-history-table)))
          (push current-text history)
          (puthash cell-id history ein:cell-history-table))
        ;; Pop from redo and apply
        (puthash cell-id (cdr redo-stack) ein:cell-redo-table)
        (ein:cell-set-text cell next-text)
        (puthash cell-id next-text ein:cell-last-snapshot-table)
        (message "Cell redone (%d more in redo)" (length (cdr redo-stack))))
    (message "No cell redo available")))

(defun ein:cell-history-clear ()
  "Clear history for current cell."
  (interactive)
  (when-let* ((cell (ein:worksheet-get-current-cell :noerror t))
              (cell-id (slot-value cell 'cell-id)))
    (remhash cell-id ein:cell-history-table)
    (remhash cell-id ein:cell-redo-table)
    (remhash cell-id ein:cell-last-snapshot-table)
    (message "Cell history cleared")))

(defun ein:cell-history-clear-all ()
  "Clear all cell history tables."
  (interactive)
  (clrhash ein:cell-history-table)
  (clrhash ein:cell-redo-table)
  (clrhash ein:cell-last-snapshot-table)
  (message "All cell history cleared"))

(provide 'ein-cell-history)

;;; ein-cell-history.el ends here
