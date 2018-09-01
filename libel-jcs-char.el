;;; libel-jcs-char.el --- Character related functions.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-09-01 22:14:27

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


;; TOPIC: Check if a character (not string) is lowercase,
;; uppercase, alphanumeric?
;; SOURCE: https://stackoverflow.com/questions/27798296/check-if-a-character-not-string-is-lowercase-uppercase-alphanumeric
(defun wordp (c) (= ?w (char-syntax c)))
(defun lowercasep (c) (and (wordp c) (= c (downcase c))))
(defun uppercasep (c) (and (wordp c) (= c (upcase c))))
(defun whitespacep (c) (= 32 (char-syntax c)))

(defun jcs-is-digit-string (c)
  "Check if C is a digit."
  (string-match-p "\^[0-9]'" c))

(defun jcs-current-char-a-wordp ()
  "Check if current character a usual letter."
  (let ((current-char nil)
        (current-char-string nil)
        (current-char-char nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    (setq current-char-char (string-to-char current-char-string))
    (wordp current-char-char)))

(defun jcs-current-char-uppercasep()
  "Check if current character a uppercase character?"
  (let ((current-char nil)
        (current-char-string nil)
        (current-char-char nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    (setq current-char-char (string-to-char current-char-string))
    (uppercasep current-char-char)))

(defun jcs-current-char-lowercasep ()
  "Check if current character a lowercase character?"
  (not (jcs-current-char-uppercasep)))

(defun jcs-current-whitespacep ()
  "Check if current character a whitespace character?"
  (let ((current-char nil)
        (current-char-string nil)
        (current-char-char nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    (setq current-char-char (string-to-char current-char-string))
    (whitespacep current-char-char)))

(defun jcs-current-char-equal-p (c)
  "Check the current character equal to 'C'."
  (let ((current-char-string (string (char-before))))
    (string= current-char-string c)))

(defun jcs-current-char-string-match-p (c)
  "Check the current character string match to 'C'."
  (let ((current-char-string (string (char-before))))
    (string-match current-char-string c)))

(defun jcs-get-current-char-byte ()
  "Get the current character as the 'byte'."
  (let ((current-char nil)
        (current-char-string nil)
        (current-char-char nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    (setq current-char-char (string-to-char current-char-string))
    current-char-char))

(defun jcs-get-current-char-string ()
  "Get the current character as the 'string'."
  (let ((current-char nil)
        (current-char-string nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    current-char-string))

;;;###autoload
(defun jcs-goto-next-backward-char (&optional bnd-pt)
  "Goto the next backward character (not include space/tab).
BND-PT : limit point."
  (interactive)
  (let ((real-lmt-pt (point-min)))
    ;; If no limit point, default as `point-min'.
    (unless (equal bnd-pt nil)
      (setq real-lmt-pt bnd-pt))

    (forward-char -1)
    (while (and (>= (point) real-lmt-pt)
                (or (jcs-current-char-equal-p " ")
                    (jcs-current-char-equal-p "\t")
                    (jcs-is-beginning-of-line-p)))
      (forward-char -1))))

;;;###autoload
(defun jcs-goto-next-forward-char (&optional bnd-pt)
  "Goto the next forward character (not include space/tab).
BND-PT : boundary point."
  (interactive)
  (let ((real-lmt-pt (point-max)))

    ;; If no limit point, default as `point-max'.
    (unless (equal bnd-pt nil)
      (setq real-lmt-pt bnd-pt))

    (forward-char 1)
    (while (and (<= (point) real-lmt-pt)
                (or (jcs-current-char-equal-p " ")
                    (jcs-current-char-equal-p "\t")
                    (jcs-is-beginning-of-line-p)))
      (forward-char 1))))

(defun jcs-first-backward-char-p (ch)
  "Check the first character on the left/backward is CH or not."
  (save-excursion
    ;; NOTE(jenchiech): First fowrad a char and ready to
    ;; be check for next backward character.
    (forward-char 1)
    (jcs-goto-next-backward-char (1+ (jcs-get-beginning-of-line-point)))
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-first-forward-char-p (ch)
  "Check the first character on the right/forward is CH or not."
  (save-excursion
    (jcs-goto-next-forward-char (jcs-get-end-of-line-point))
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-is-there-char-backward-point-p (pt)
  "Check if there is at least one character backward until \
the point.
PT : point."
  (save-excursion
    (jcs-goto-next-backward-char pt)
    (>= (point) pt)))

(defun jcs-is-there-char-forward-point-p (pt)
  "Check if there is at least one character forward until \
the point.
PT : point."
  (save-excursion
    (jcs-goto-next-forward-char pt)
    (<= (point) pt)))

(defun jcs-is-there-char-backward-util-beginning-of-line-p ()
  "Check if there are at least a character on the left until \
the beginning of the line."
  (interactive)
  (jcs-is-there-char-backward-point-p (jcs-get-beginning-of-line-point)))

(defun jcs-is-there-char-forward-until-end-of-line-p ()
  "Check if there are at least a character on the right until \
the end of the line."
  (interactive)
  (jcs-is-there-char-forward-point-p (jcs-get-end-of-line-point)))


(provide 'libel-jcs-char)
;;; libel-jcs-char.el ends here
