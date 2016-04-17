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

(require 'utf8-decode)
(require 'ring)

(defgroup readline nil
  "Readline-style editing library"
  :group 'terminals
  :prefix "rl-")

(defcustom rl-decode-utf8 t
  "If non-nil, try decoding UTF-8 codepoints.
Disable this if you're having issues or are using a different
multi-byte encoding."
  :type 'boolean
  :group 'readline)

;;; input

(defun rl-read-byte ()
  (string-to-number (read-from-minibuffer "") 16))

(defun rl-read-utf8-char ()
  (let ((state 0)
        (code-point 0)
        complete decoded)
    (while (not complete)
      (setq decoded (utf8-decode state code-point (rl-read-byte))
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

(defun rl-read-sequence ()
  (let ((state 'start)
        char output)
    (while (not (eq state 'end))
      (setq char (funcall (if rl-decode-utf8 'rl-read-utf8-char 'read-byte)))
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

;;; history handling

(defvar rl-history-size 100)
(defvar rl-history-index 0)
(defvar rl-history (make-ring rl-history-size))

;; not sure why that's not a thing...
(defun rl-ring-set (ring index item)
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

(defun rl-history-add (line)
  (ring-insert rl-history line))

(defun rl-history-dump (file)
  (let ((base-dir (file-name-directory file)))
    (when (not (file-exists-p base-dir))
      (make-directory base-dir)))

  (let ((buffer-file-coding-system 'raw-text))
    (with-temp-file file
      (dolist (item (nreverse (ring-elements rl-history)))
        (insert (format "%s\n" item))))))

(defun rl-history-load (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring (line-beginning-position)
                                    (line-end-position))))
        (ring-insert rl-history line)
        (forward-line 1)))))

;;; escape codes

;; see https://github.com/antirez/linenoise and console_codes(4)

(defconst rl-delete-to-end-sequence "\e[0K")
(defconst rl-retreat-cursor-sequence "\e[%sD")
(defconst rl-advance-cursor-sequence "\e[%sC")
(defconst rl-clear-screen-sequence "\e[H\e[2J")

;;; main logic

;; TODO add shim for emacs 24.3
(define-error 'rl-cancel "Input cancelled")

(defun rl-redraw-screen (prompt)
  (princ rl-clear-screen-sequence)
  (princ prompt)
  (princ (buffer-string)))

(defun rl-navigate-history (prompt &optional prev)
  (let ((rl-history-length (ring-length rl-history)))
    (when (and (> rl-history-length 1)
               (if prev
                   (< rl-history-index (1- rl-history-length))
                 (> rl-history-index 0)))
      (rl-ring-set rl-history rl-history-index (buffer-string))
      (setq rl-history-index (+ rl-history-index (if prev 1 -1)))
      (let ((other-item (ring-ref rl-history rl-history-index)))
        (when (not (bobp))
          (princ (format rl-retreat-cursor-sequence (1- (point)))))
        (princ rl-delete-to-end-sequence)
        (princ other-item)
        (erase-buffer)
        (insert other-item)))))

(defun rl-prev-char ()
  (when (not (bobp))
    (princ (format rl-retreat-cursor-sequence 1))
    (forward-char -1)))

(defun rl-next-char ()
  (when (not (eobp))
    (princ (format rl-advance-cursor-sequence 1))
    (forward-char 1)))

(defun rl-line-beginning (prompt)
  (when (not (bobp))
    (princ (format rl-retreat-cursor-sequence (1- (point))))
    (goto-char (point-min))))

(defun rl-line-end ()
  (when (not (eobp))
    (princ (format rl-advance-cursor-sequence (- (point-max) (point))))
    (goto-char (point-max))))

(defun rl-delete-prev-char ()
  (when (not (bobp))
    (princ (format rl-retreat-cursor-sequence 1))
    (princ rl-delete-to-end-sequence)
    (when (not (eobp))
      (princ (buffer-substring (point) (point-max)))
      (princ (format rl-retreat-cursor-sequence (- (point-max) (point)))))
    (delete-char -1)))

(defun rl-delete-next-char ()
  (when (not (eobp))
    (princ rl-delete-to-end-sequence)
    (when (> (- (point-max) (point)) 1)
      (princ (buffer-substring (1+ (point)) (point-max)))
      (princ (format rl-retreat-cursor-sequence (- (point-max) (1+ (point))))))
    (delete-char 1)))

(defun rl-delete-line (prompt)
  (when (not (zerop (buffer-size)))
    (when (not (bobp))
      (princ (format rl-retreat-cursor-sequence (1- (point)))))
    (princ rl-delete-to-end-sequence)
    (erase-buffer)))

(defun rl-delete-to-end ()
  (when (not (eobp))
    (delete-region (point) (point-max))
    (princ rl-delete-to-end-sequence)))

(defun rl-delete-prev-word ()
  (when (not (bobp))
    (let ((word-beg (save-excursion
                      (forward-word -1)
                      (point))))
      (princ (format rl-retreat-cursor-sequence (- (point) word-beg)))
      (princ rl-delete-to-end-sequence)
      (when (not (eobp))
        (princ (buffer-substring (point) (point-max)))
        (princ (format rl-retreat-cursor-sequence (- (point-max) (point)))))
      (delete-region word-beg (point)))))

(defun rl-transpose-chars ()
  (when (>= (buffer-size) 2)
    (cond
     ((bobp)
      (princ (format rl-advance-cursor-sequence 1))
      (forward-char 1))
     ((eobp)
      (princ (format rl-retreat-cursor-sequence 1))
      (forward-char -1)))
    (let ((prev (preceding-char))
          (next (following-char)))
      (princ (format rl-retreat-cursor-sequence 1))
      (princ (format "%c%c" next prev))
      (delete-char -1)
      (delete-char 1)
      (insert next prev))))

(defun rl-send-line ()
  (princ "\n")
  (setq rl-history-index 0)
  (ring-remove rl-history 0))

(defun rl-cancel-input ()
  (princ "\n")
  (signal 'rl-cancel nil))

(defun rl-printed-representation (chars)
  (mapconcat
   (lambda (char)
     (if (< char ?\s)
         (format "^%c" (logior char 64))
       (format "%c" char)))
   chars ""))

(defun rl-print-chars (chars)
  (let ((representation (rl-printed-representation chars)))
    (princ representation)
    (when (> (- (point-max) (point)) 1)
      (princ rl-delete-to-end-sequence)
      (princ (buffer-substring (point) (point-max)))
      (princ (format rl-retreat-cursor-sequence (- (point-max) (point)))))
    (insert representation)))

(defun rl-read-line (prompt)
  (with-temp-buffer
    (princ prompt)
    (let (eol return)
      (ring-insert rl-history "") ; scratch entry
      (while (not (or eol return))
        (let* ((chars (rl-read-sequence)))
          (cond
           ;; special
           ((equal chars "\C-l")
            (rl-redraw-screen prompt))
           ;; history
           ((or (equal chars "\C-p")
                (equal chars "\e[A")) ; <up>
            (rl-navigate-history prompt t))
           ((or (equal chars "\C-n")
                (equal chars "\e[B")) ; <down>
            (rl-navigate-history prompt))
           ;; movement
           ((or (equal chars "\C-b")
                (equal chars "\e[D")) ; <left>
            (rl-prev-char))
           ((or (equal chars "\C-f")
                (equal chars "\e[C")) ; <right>
            (rl-next-char))
           ((or (equal chars "\C-a")
                (equal chars "\e[H") ; <home> (xterm)
                (equal chars "\e[1~"))
            (rl-line-beginning prompt))
           ((or (equal chars "\C-e")
                (equal chars "\e[F") ; <end> (xterm)
                (equal chars "\e[4~"))
            (rl-line-end))
           ((or (equal chars "\C-h")
                (equal chars "\C-?"))
            (rl-delete-prev-char))
           ((equal chars "\C-d")
            (if (zerop (buffer-size)) ; empty prompt
                (progn
                  (setq eol t)
                  (ring-remove rl-history 0))
              (rl-delete-next-char)))
           ((equal chars "\e[3~") ; DEL
            (rl-delete-next-char))
           ((equal chars "\C-u")
            (rl-delete-line prompt))
           ((equal chars "\C-k")
            (rl-delete-to-end))
           ((equal chars "\C-w")
            (rl-delete-prev-word))
           ;; editing
           ((equal chars "\C-t")
            (rl-transpose-chars))
           ;; control flow
           ((or (equal chars "\C-j")
                (equal chars "\C-m"))
            (rl-send-line)
            (setq return t))
           ((equal chars "\C-c")
            (rl-cancel-input))
           ;; printables
           (t
            (rl-print-chars chars)))))
      (when return
        (buffer-string)))))

(provide 'readline)
