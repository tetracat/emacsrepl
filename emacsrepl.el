;; Copyright (C) 2016 Alain Kalker <a.c.kalker@gmail.com>
;; Copyright (C) 2016 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; https://github.com/antirez/linenoise
;; console_codes(4)

(defvar column-start "\e[0G")
(defvar delete-to-end "\e[0K")
(defvar retreat-cursor "\e[%sD")
(defvar advance-cursor "\e[%sC")
(defvar clear-screen "\e[H\e[2J")

;; TODO turn into a defcustom after namespacing
(defvar decode-utf8 t)
(defvar read-char-function
  (if decode-utf8
      'read-utf8-char
    'read-byte))

(defun read-byte ()
  (string-to-number (read-from-minibuffer "") 16))

(defun read-utf8-char ()
  (let ((state 0)
        (code-point 0)
        complete decoded)
    (while (not complete)
      (setq decoded (utf8-decode state code-point (read-byte))
            state (car decoded)
            code-point (cdr decoded))
      (cond
       ((= state 0)
        (setq complete t))
       ((= state 1) ; fail gracefully for single-byte compatibility
        (setq state 0))
       (t
        ;; continue decoding non-trivial sequences
        )))
    code-point))

(defun read-sequence ()
  ;; TODO test out `locale charmap` and `iconv -t UTF-32 -`
  ;; NOTE how would one convert from utf-32 to utf-8 though?
  (let ((state 'start)
        char output)
    (while (not (eq state 'end))
      (setq char (funcall read-char-function))
      (push char output)
      (cond
       ((eq state 'start)
        (if (= char ?\e)
            (setq state 'escape)
          (setq state 'end)))
       ((eq state 'escape)
        (if (= char ?\[)
            (setq state 'csi)
          (setq state 'end)))
       ((eq state 'csi)
        (if (and (>= char ?0) (<= char ?9))
            (setq state 'csi)
          (setq state 'end)))))
    (concat (nreverse output))))

(defun char-unprintable-p (char)
  (< char ?\s))

(defun printed-representation (chars)
  (mapconcat
   (lambda (char)
     (if (char-unprintable-p char)
         (cond
          ((< char ?\s)
           (format "^%c" (logior char 64)))
          ((= char ?\C-?)
           "^?")
          ((> char ?\C-?)
           (format "\\%o" char)))
       (format "%c" char)))
   chars ""))

(define-error 'readline-cancel "Input cancelled")

(defun read-line (prompt)
  (with-temp-buffer
    (princ prompt)
    (let (eol return)
      (while (not (or eol return))
        (let* ((chars (read-sequence)))
          (cond
           ((equal chars "\C-l")
            (princ clear-screen)
            (princ prompt)
            (princ (buffer-string)))
           ((or (equal chars "\C-b")
                (equal chars "\e[D")) ; <left>
            (when (not (bobp))
              (princ (format retreat-cursor 1))
              (forward-char -1)))
           ((or (equal chars "\C-f")
                (equal chars "\e[C")) ; <right>
            (when (not (eobp))
              (princ (format advance-cursor 1))
              (forward-char 1)))
           ((or (equal chars "\C-a")
                (equal chars "\e[H")) ; <home> (xterm)
            (when (not (bobp))
              (princ column-start)
              (princ (format advance-cursor (length prompt)))
              (goto-char (point-min))))
           ((or (equal chars "\C-e")
                (equal chars "\e[F")) ; <end> (xterm)
            (when (not (eobp))
              (princ (format advance-cursor (- (point-max) (point))))
              (goto-char (point-max))))
           ((or (equal chars "\C-h")
                (equal chars "\C-?"))
            (when (not (bobp))
              (princ (format retreat-cursor 1))
              (princ delete-to-end)
              (when (not (eobp))
                (princ (buffer-substring (point) (point-max)))
                (princ (format retreat-cursor (- (point-max) (point)))))
              (delete-char -1)))
           ((equal chars "\C-d")
            (if (zerop (buffer-size)) ; empty prompt
                (setq eol t)
              (when (not (eobp))
                (princ delete-to-end)
                (when (> (- (point-max) (point)) 1)
                  (princ (buffer-substring (1+ (point)) (point-max)))
                  (princ (format retreat-cursor (- (point-max) (1+ (point))))))
                (delete-char 1))))
           ((equal chars "\e[3~") ; DEL
            (when (not (eobp))
              (princ delete-to-end)
              (when (> (- (point-max) (point)) 1)
                (princ (buffer-substring (1+ (point)) (point-max)))
                (princ (format retreat-cursor (- (point-max) (1+ (point))))))
              (delete-char 1)))
           ((or (equal chars "\C-j")
                (equal chars "\C-m"))
            (princ "\n")
            (setq return t))
           ((equal chars "\C-c")
            (princ "\n")
            (signal 'readline-cancel nil))
           (t
            (let ((representation (printed-representation chars)))
              (princ representation)
              (when (> (- (point-max) (point)) 1)
                (princ delete-to-end)
                (princ (buffer-substring (point) (point-max)))
                (princ (format retreat-cursor (- (point-max) (point)))))
              (insert representation))))))
        (if return
            (buffer-string)
          nil))))


(defun rep (input)
  (let ((form (read input)))
    (prin1-to-string (eval form))))

(defun emacs-read-line (prompt)
  (ignore-errors (read-from-minibuffer prompt)))

(defvar readline-function
  (if (member (getenv "TERM") '("dumb" "cons25" "emacs"))
      'emacs-read-line
    'read-line))

(defun repl ()
  (let (eof)
    (while (not eof)
      (condition-case err
          (let ((line (funcall readline-function "EMACS> ")))
            (if line
                (princ (format "%s\n" (rep line)))
              (princ "\n")
              (setq eof t)))
        (readline-cancel) ; proceed with new prompt
        (error
         (princ (format "%s\n" (error-message-string err))))))))

(repl)
