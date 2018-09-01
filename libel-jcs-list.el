;;; libel-jcs-list.el --- List related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 22:04:37

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


(defun jcs-flatten-list (l)
  "Flatten the multiple dimensional array to one dimensonal array.
'(1 2 3 4 (5 6 7 8)) => '(1 2 3 4 5 6 7 8).
L : list we want to flaaten."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (jcs-flatten-list a)))))

(defun jcs-is-in-list-string (in-list str)
  "Check if a string in the string list.
IN-LIST : list of strings.
STR : string to check if is inside the list of strings above."
  (cl-some #'(lambda (lb-sub-str) (string-match lb-sub-str str)) in-list))


(provide 'libel-jcs-list)
;;; libel-jcs-list.el ends here
