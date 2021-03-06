;;; libel-jcs-file.el --- File related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 21:55:07

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

(defun jcs-get-file-name ()
  "Get current file name."
  (file-name-nondirectory buffer-file-name))

(defun jcs-get-file-name-uppercase ()
  "Get current file name uppercase."
  (upcase (file-name-nondirectory buffer-file-name)))

(defun jcs-get-file-name-lowercase ()
  "Get current file name uppercase."
  (downcase (file-name-nondirectory buffer-file-name)))

(defun jcs-get-file-name-without-extension ()
  "Get current file name without extension."
  (file-name-sans-extension (file-name-nondirectory buffer-file-name)))

(defun jcs-get-file-name-without-extension-uppercase ()
  "Get current file name without extension."
  (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

(defun jcs-get-file-name-without-extension-lowercase ()
  "Get current file name without extension."
  (downcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))


(provide 'libel-jcs-file)
;;; libel-jcs-file.el ends here
