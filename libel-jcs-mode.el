;;; libel-jcs-mode.el --- Mode related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 22:00:27

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

(defun jcs-is-current-major-mode-p (str)
  "Check if this major mode.
STR : major mode name."
  (string= major-mode str))

(defun jcs-is-minor-mode-enabled-p (mode-obj)
  "Check if this minor enabled in current buffer/file.
MODE-OBJ : mode object memory."
  (bound-and-true-p mode-obj))


(provide 'libel-jcs-mode)
;;; libel-jcs-mode.el ends here
