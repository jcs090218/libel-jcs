;;; libel-jcs-line.el --- Line related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 22:18:14

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;;;###autoload
(defun jcs-goto-first-char-in-line ()
  "Goto beginning of line but ignore 'empty characters'(spaces/tabs)."
  (interactive)
  (jcs-back-to-indentation-or-beginning)
  (when (jcs-is-beginning-of-line-p)
    (jcs-back-to-indentation-or-beginning)))

(defun jcs-current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]\t]*$")))

(defun jcs-current-line-totally-empty-p ()
  "Current line empty with no spaces/tabs in there.  (absolute)."
  (and (jcs-is-beginning-of-line-p)
       (jcs-is-end-of-line-p)))

(defun jcs-current-line-comment-p ()
  "Check if current line only comment."
  (save-excursion
    (let ((is-comment-line nil))
      (end-of-line)
      (when (or (jcs-is-inside-comment-block-p)
                (jcs-current-line-empty-p))
        (setq is-comment-line t))
      is-comment-line)))

(defun jcs-get-beginning-of-line-point (&optional ln)
  "Return point at beginning of current line.
LN : line number."
  (save-excursion
    (unless (equal ln nil)
      (goto-line ln))
    (beginning-of-line)
    (point)))

(defun jcs-get-end-of-line-point (&optional ln)
  "Return point at end of current line.
LN : line number."
  (save-excursion
    (unless (equal ln nil)
      (goto-line ln))
    (end-of-line)
    (point)))

(defun jcs-is-end-of-line-p ()
  "Is at the end of line?"
  (= (point) (jcs-get-end-of-line-point)))

(defun jcs-is-end-of-buffer-p ()
  "Is at the end of buffer?"
  (= (point) (point-max)))

(defun jcs-is-beginning-of-line-p ()
  "Is at the beginning of line?"
  (= (current-column) 0))

(defun jcs-is-beginning-of-buffer-p ()
  "Is at the beginning of buffer?"
  (= (point) (point-min)))

(defun jcs-is-current-file-empty-p ()
  "Check if the file a empty file."
  (and (jcs-is-beginning-of-buffer-p)
       (jcs-is-end-of-buffer-p)))

(defun jcs-get-current-line-integer ()
  "Get the current line as integer."
  (string-to-number (jcs-get-current-line-string)))

(defun jcs-get-current-line-string ()
  "Get the current line as string."
  (format-mode-line "%l"))

(defun jcs-is-current-line (line)
  "Is current line number this line?
LINE : number to check if current line this line?"
  (= (string-to-number (format-mode-line "%l")) line))

(defun jcs-is-at-start-of-line-p ()
  "Cursor is at the first character of this line?"
  (let ((current-point nil)
        (firstCharPoint nil))
    (save-excursion
      (setq current-point (point))
      (back-to-indentation)
      (setq firstCharPoint (point)))

    (= firstCharPoint current-point)))

(defun jcs-is-met-first-char-at-line-p ()
  "Check current cursor point is after the first character at \
the current line.

@return { boolean } : true, infront of first character.
false, vice versa."
  (let ((is-infront-of-first-char t)
        (point-to-check nil))
    (save-excursion
      (ignore-errors
        (setq point-to-check (point))
        (beginning-of-line)

        (when (not (jcs-current-line-totally-empty-p))
          (forward-char 1))

        (while (<= (point) point-to-check)
          (if (not (current-whitespacep))
              (setq is-infront-of-first-char nil))
          (forward-char 1))))

    (eq is-infront-of-first-char t)))

(defun jcs-empty-line-between-point (min-pt max-pt)
  "Check if there is empty line between two point.
MIN-PT : smaller position.
MAX-PT : larger position."
  (save-excursion
    (let ((there-is-empty-line nil))
      (when (>= min-pt max-pt)
        (jcs-warning "Min point cannot be larger than max point..")
        ;; Return false.
        (equal there-is-empty-line t))

      (goto-char min-pt)
      (while (< (point) max-pt)
        (when (jcs-current-line-empty-p)
          ;; Return true.
          (setq there-is-empty-line t)
          (equal there-is-empty-line t))
        (jcs-next-line))
      ;; Return false.
      (equal there-is-empty-line t))))

;;;###autoload
(defun jcs-safe-forward-char ()
  "Forward a char if not the end of the line."
  (if (not (jcs-is-beginning-of-line-p))
      (forward-char 1)))

;;;###autoload
(defun jcs-safe-backward-char ()
  "Backward a char if not the beginning of the line."
  (if (not (jcs-is-end-of-line-p))
      (backward-char 1)))


(defun jcs-start-line-in-buffer ()
  "Is current line the start line in buffer."
  (let ((buffer-start-line-num nil)
        ;; Get the current line number in the shell buffer.
        (current-line-num (jcs-get-current-line-integer)))
    ;; Get the last line number in the current shell buffer.
    (save-excursion
      (goto-char (point-min))
      (setq buffer-start-line-num (jcs-get-current-line-integer)))

    ;; Return it.
    (= current-line-num buffer-start-line-num)))

(defun jcs-last-line-in-buffer ()
  "Is current line the last line in buffer."
  (let ((buffer-last-line-num nil)
        ;; Get the current line number in the shell buffer.
        (current-line-num (jcs-get-current-line-integer)))
    ;; Get the last line number in the current shell buffer.
    (save-excursion
      (goto-char (point-max))
      (setq buffer-last-line-num (jcs-get-current-line-integer)))

    ;; Return it.
    (= current-line-num buffer-last-line-num)))


(provide 'libel-jcs-line)
;;; libel-jcs-line.el ends here
