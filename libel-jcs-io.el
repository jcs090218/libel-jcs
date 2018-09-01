;;; libel-jcs-io.el --- I/O related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 21:58:06

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


(defun jcs-get-string-from-file (filePath)
  "Return filePath's file content.
TOPIC(jenchieh): Elisp: Read File Content as String or List of Lines
URL(jenchieh): http://ergoemacs.org/emacs/elisp_read_file_content.html

FILEPATH : file path."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun jcs-project-write-file (in-filename in-content)
  "Write to a file in the project root.
IN-FILENAME : path to write, is relative path to project root.
IN-CONTENT : content/buffer to write to the IN-FILENAME."
  (write-region in-content  ;; Start
                nil  ;; End
                ;; File name (concatenate full path)
                (concat (cdr (project-current))
                        in-filename)  ;; Cache filename.
                ;; Overwrite?
                t))

(defun jcs-parse-ini (filePath)
  "Parse a .ini file.
FILEPATH : .ini file to parse."

  (let ((tmp-ini (jcs-get-string-from-file filePath))
        (tmp-ini-list '())
        (tmp-pair-list nil)
        (tmp-keyword "")
        (tmp-value "")
        (count 0))
    (setq tmp-ini (split-string tmp-ini "\n"))

    (dolist (tmp-line tmp-ini)
      ;; check not comment.
      (when (not (string-match-p "#" tmp-line))
        ;; Split it.
        (setq tmp-pair-list (split-string tmp-line "="))

        ;; Assign to temporary variables.
        (setq tmp-keyword (nth 0 tmp-pair-list))
        (setq tmp-value (nth 1 tmp-pair-list))

        ;; Check empty value.
        (when (and (not (string= tmp-keyword ""))
                   (not (equal tmp-value nil)))
          (let ((tmp-list '()))
            (add-to-list 'tmp-list tmp-keyword)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))
          (let ((tmp-list '()))
            (add-to-list 'tmp-list tmp-value)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))))
      (setq count (1+ count)))

    ;; return list.
    tmp-ini-list))

(defun jcs-get-properties (ini-list in-key)
  "Get properties data.  Search by key and returns value.
INI-LIST : ini list.  Please use this with/after using `jcs-parse-ini' function.
IN-KEY : key to search for value."
  (let ((tmp-index 0)
        (tmp-key "")
        (tmp-value "")
        (returns-value ""))

    (while (< tmp-index (length ini-list))
      ;; Get the key and data value.
      (setq tmp-key (nth tmp-index ini-list))
      (setq tmp-value (nth (1+ tmp-index) ini-list))

      ;; Find the match.
      (when (string= tmp-key in-key)
        ;; return data value.
        (setq returns-value tmp-value))

      ;; Search for next key word.
      (setq tmp-index (+ tmp-index 2)))

    ;; Found nothing, return empty string.
    returns-value))


(provide 'libel-jcs-io)
;;; libel-jcs-io.el ends here
