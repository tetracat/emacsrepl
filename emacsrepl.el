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

(defvar input-history-file
  (let ((data-home (getenv "XDG_DATA_HOME")))
    (if (and data-home (= (aref data-home 0) ?/))
        (concat data-home "emacsrepl/history")
      (expand-file-name "~/.local/share/emacsrepl/history"))))

(defvar input-history-size 100)
(defvar input-history-index 0)
(defvar input-history (make-ring input-history-size))

;; not sure why that's not a thing...
(defun ring-set (ring index item)
  "Set RING's INDEX element to ITEM.
INDEX = 0 is the most recently inserted; higher indices
correspond to older elements.
INDEX need not be <= the ring length; the appropriate modulo operation
will be performed."
  (if (ring-empty-p ring)
      (error "Accessing an empty ring")
    (let ((hd (car ring))
	  (ln (cadr ring))
	  (vec (cddr ring)))
      (aset vec (ring-index index hd ln (length vec)) item))))

(defun input-history-dump (file)
  (let ((base-dir (file-name-directory file)))
    (when (not (file-exists-p base-dir))
      (make-directory base-dir)))
  (with-temp-file file
    (dolist (item (nreverse (ring-elements input-history)))
      (insert (format "%s\n" item)))))

(defun input-history-load (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring (line-beginning-position)
                                    (line-end-position))))
        (ring-insert input-history line)
        (forward-line 1)))))

(defun read-line (prompt)
  (with-temp-buffer
    (princ prompt)
    (let (eol return)
      (ring-insert input-history "") ; scratch entry
      (while (not (or eol return))
        (let* ((chars (read-sequence)))
          (cond
           ((equal chars "\C-l")
            (princ clear-screen)
            (princ prompt)
            (princ (buffer-string)))
           ((or (equal chars "\C-p")
                (equal chars "\e[A")) ; <up>
            (let ((input-history-length (ring-length input-history)))
              (when (and (> input-history-length 1)
                         (< input-history-index (1- input-history-length)))
                (ring-set input-history input-history-index (buffer-string))
                (setq input-history-index (1+ input-history-index))
                (let ((prev-item (ring-ref input-history input-history-index)))
                  (princ column-start)
                  (princ delete-to-end)
                  (princ prompt)
                  (princ prev-item)
                  (erase-buffer)
                  (insert prev-item)))))
           ((or (equal chars "\C-n")
                (equal chars "\e[B")) ; <down>
            (let ((input-history-length (ring-length input-history)))
              (when (and (> input-history-length 1)
                         (> input-history-index 0))
                (ring-set input-history input-history-index (buffer-string))
                (setq input-history-index (1- input-history-index))
                (let ((next-item (ring-ref input-history input-history-index)))
                  (princ column-start)
                  (princ delete-to-end)
                  (princ prompt)
                  (princ next-item)
                  (erase-buffer)
                  (insert next-item)))))
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
                (equal chars "\e[H") ; <home> (xterm)
                (equal chars "\e[1~"))
            (when (not (bobp))
              (princ column-start)
              (princ (format advance-cursor (length prompt)))
              (goto-char (point-min))))
           ((or (equal chars "\C-e")
                (equal chars "\e[F") ; <end> (xterm)
                (equal chars "\e[4~"))
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
                (progn
                  (setq eol t)
                  (ring-remove input-history 0))
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
           ((equal chars "\C-k")
            (when (not (eobp))
              (delete-region (point) (point-max))
              (princ delete-to-end)))
           ((equal chars "\C-u")
            (when (not (zerop (buffer-size)))
              (erase-buffer)
              (princ column-start)
              (princ (format advance-cursor (length prompt)))
              (princ delete-to-end)))
           ((equal chars "\C-w")
            (when (not (bobp))
              (let ((word-beg (save-excursion
                                (forward-word -1)
                                (point))))
                (princ (format retreat-cursor (- (point) word-beg)))
                (princ delete-to-end)
                (when (not (eobp))
                  (princ (buffer-substring (point) (point-max)))
                  (princ (format retreat-cursor (- (point-max) (point)))))
                (delete-region word-beg (point)))))
           ((equal chars "\C-t")
            (when (>= (buffer-size) 2)
              (cond
               ((bobp)
                (princ (format advance-cursor 1))
                (forward-char 1))
               ((eobp)
                (princ (format retreat-cursor 1))
                (forward-char -1)))
              (let ((prev (preceding-char))
                     (next (following-char)))
                (princ (format retreat-cursor 1))
                (princ (format "%c%c" next prev))
                (delete-char -1)
                (delete-char 1)
                (insert next prev))))
           ((or (equal chars "\C-j")
                (equal chars "\C-m"))
            (princ "\n")
            (setq return t
                  input-history-index 0)
            (ring-remove input-history 0))
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
      (when return
        (buffer-string)))))

(defun rep (input)
  (let ((form (read input)))
    (prin1-to-string (eval form))))

(defun emacs-read-line (prompt)
  (ignore-errors (read-from-minibuffer prompt)))

(defvar dumb-term-p (member (getenv "TERM") '("dumb" "cons25" "emacs")))

(defvar readline-function
  (if dumb-term-p
      'emacs-read-line
    'read-line))

(defvar blank-or-comment-re
  (rx bos (* space) (? (: ";" (* any))) eos))

(defun repl ()
  (let (eof)
    (when (and (not dumb-term-p) (file-exists-p input-history-file))
      (input-history-load input-history-file))
    (while (not eof)
      (condition-case err
          (let ((line (funcall readline-function "EMACS> ")))
            (if line
                (if (string-match-p blank-or-comment-re line)
                    ;; treat empty line like C-c
                    (signal 'readline-cancel nil)
                  (when (not dumb-term-p)
                    (ring-insert input-history line))
                  (princ (format "%s\n" (rep line))))
              (princ "\n")
              (setq eof t)))
        (readline-cancel) ; proceed with new prompt
        (error
         (princ (format "%s\n" (error-message-string err))))))
    (when (not dumb-term-p)
      (input-history-dump input-history-file))))

(repl)
