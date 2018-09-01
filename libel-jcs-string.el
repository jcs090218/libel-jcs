;;; libel-jcs-string.el --- String related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 21:45:47

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

(require 'cl-lib)


(defun jcs-remove-string-by-substring (str substr)
  "Remove a certain string by a substring.
STR : string to want to be remove by SUBSTR.
SUBSTR : string you want to remove from STR."
  (s-replace substr "" str))

(defun jcs-replace-string (rp-tar rp-str str)
  "Replace a string from another string.
STR : main string to change.
RP-TAR : target string we want to remove from STR.
RP-STR : replace string will be set into STR."
  (s-replace rp-tar rp-str str))

(defun jcs-contain-string (in-sub-str in-str)
  "Check if a string is a substring of another string.
Return true if contain, else return false.
IN-SUB-STR : substring to see if contain in the IN-STR.
IN-STR : string to check by the IN-SUB-STR."
  (string-match-p (regexp-quote in-sub-str) in-str))

(defun jcs-is-contain-list-string (in-list in-str)
  "Check if a string contain in any string in the string list.
IN-LIST : list of string use to check if IN-STR in contain one of
the string.
IN-STR : string using to check if is contain one of the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))


(provide 'libel-jcs-string)
;;; libel-jcs-string.el ends here
