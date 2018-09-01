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


(defun jcs-get-word-at-point ()
  "Get word at current cursor position."
  (interactive)
  (thing-at-point 'word))

(defun jcs-current-word-equal-p (str)
  "Check the current word equal to 'STR'."
  (string= (thing-at-point 'word) str))


(provide 'libel-jcs-word)
;;; libel-jcs-word.el ends here
