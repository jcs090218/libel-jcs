;;; libel-jcs-nav.el --- Navigation related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 21:53:48

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
(defun jcs-previous-blank-line ()
  "Move to the previous line containing nothing but whitespaces or tabs."
  (interactive)
  (unless (ignore-errors (or (search-backward-regexp "^[ \t]*\n") t))
    (goto-char (point-min))))

;;;###autoload
(defun jcs-next-blank-line ()
  "Move to the next line containing nothing but whitespaces or tabs."
  (interactive)
  (forward-line)
  (if (ignore-errors (or (search-forward-regexp "^[ \t]*\n") t))
      (forward-line -1)
    (goto-char (point-max))))


(provide 'libel-jcs-nav)
;;; libel-jcs-nav.el ends here
