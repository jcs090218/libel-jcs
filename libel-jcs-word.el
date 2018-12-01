;;; libel-jcs-word.el --- Word related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 22:16:30

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
(defun jcs-print-current-word ()
  "Print out the current word."
  (interactive)
  (message "Current word: %s" (jcs-get-word-at-point)))

(defun jcs-get-word-at-point ()
  "Get word at current cursor position."
  (interactive)
  (thing-at-point 'word))

(defun jcs-current-word-equal-p (str)
  "Check the current word equal to 'STR'."
  (string= (thing-at-point 'word) str))

;;;###autoload
(defun jcs-goto-first-backward-word ()
  "Goto the first backward word."
  (interactive)
  (unless (jcs-current-whitespace-or-tab-p)
    (backward-word 1))
  (backward-word 1))

;;;###autoload
(defun jcs-goto-first-forward-word ()
  "Goto the first forward word."
  (interactive)
  (unless (jcs-current-whitespace-or-tab-p)
    (forward-word 1))
  (forward-word 1))

(defun jcs-first-backward-word ()
  "Find out the first backward word from the current cursor position."
  (let ((word ""))
    (save-excursion
      (call-interactively #'jcs-goto-first-backward-word)
      (setq word (jcs-get-word-at-point)))
    word))

(defun jcs-first-forward-word ()
  "Find out the first backward word from the current cursor position."
  (let ((word ""))
    (save-excursion
      (call-interactively #'jcs-goto-first-forward-word)
      (setq word (jcs-get-word-at-point)))
    word))

(defun jcs-first-backward-word-p (w)
  "Find out the first backward word from the current cursor position and \
compare W.
Returns non-nil, the word is the same.
Returns nil, the word isn't the same."
  (string= w (jcs-first-backward-word)))

(defun jcs-first-forward-word-p (w)
  "Find out the first forward word from the current cursor position and \
compare W.
Returns non-nil, the word is the same.
Returns nil, the word isn't the same."
  (string= w (jcs-first-forward-word)))


(provide 'libel-jcs-word)
;;; libel-jcs-word.el ends here
