;;; libel-jcs-face.el --- Face related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 22:02:32

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

(require 'libel-jcs-list)

;;;###autoload
(defun jcs-what-face ()
  "Print out all the faces the current cursor on."
  (interactive)
  (message "Current faces: %s" (jcs-get-current-point-face)))

(defun jcs-get-faces (pos)
  "Get the font faces at POS."
  (jcs-flatten-list
   (remq nil
         (list
          (get-char-property pos 'read-face-name)
          (get-char-property pos 'face)
          (plist-get (text-properties-at pos) 'face)))))

(defun jcs-get-current-point-face ()
  "Get current point's type face as string."
  (jcs-get-faces (point)))

(defun jcs-is-current-point-face (in-face)
  "Check if current face the same face as IN-FACE.
Returns, True if is the same as pass in face name string.
False, is not the same as pass in face name string.
IN-FACE : input face name as string."
  (let ((faces (jcs-get-current-point-face)))
    (if (listp faces)
        (if (equal (cl-position in-face faces :test 'string=) nil)
            ;; If return nil, mean not found in the `faces' list.
            nil
          ;; If have position, meaning the face exists.
          t)
      (string= in-face faces))))

(defun jcs-is-default-face-p ()
  "Return non-nil, if is default face.
Return nil, if not default face."
  (and (= (length (jcs-get-current-point-face)) 1)
       (or
        ;; STUDY(jenchieh): nil means `default' face, I guess.
        (jcs-is-current-point-face "nil")
        (jcs-is-current-point-face "hl-line"))))


(provide 'libel-jcs-face)
;;; libel-jcs-face.el ends here
