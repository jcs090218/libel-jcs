;;; libel-jcs-comment.el --- Comment related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 22:21:10

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


(defun jcs-is-inside-comment-block-p ()
  "Check if current cursor point inside the comment block."
  (nth 4 (syntax-ppss)))

;;;###autoload
(defun jcs-goto-start-of-the-comment ()
  "Go to the start of the comment."
  (interactive)
  (when (jcs-is-inside-comment-block-p)
    (backward-char 1)
    (jcs-goto-start-of-the-comment)))

;;;###autoload
(defun jcs-goto-end-of-the-comment ()
  "Go to the end of the comment."
  (interactive)
  (when (jcs-is-inside-comment-block-p)
    (backward-char -1)
    (jcs-goto-end-of-the-comment)))


(provide 'libel-jcs-nav)
;;; libel-jcs-comment.el ends here
