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

(require 'readline)

(defun rep (input)
  (let ((form (read input)))
    (prin1-to-string (eval form))))

(defun emacs-read-line (prompt)
  (ignore-errors (read-from-minibuffer prompt)))

(defvar dumb-term-p (member (getenv "TERM") '("dumb" "cons25" "emacs")))

(defvar readline-function
  (if dumb-term-p
      'emacs-read-line
    'rl-read-line))

(defvar history-file
  (let ((data-home (getenv "XDG_DATA_HOME")))
    (if (and data-home (= (aref data-home 0) ?/))
        (concat data-home "/emacsrepl/history")
      (expand-file-name "~/.local/share/emacsrepl/history"))))

(defvar blank-or-comment-re
  (rx bos (* space) (? (: ";" (* any))) eos))

(defun repl ()
  (let (eof)
    (when (and (not dumb-term-p) (file-exists-p history-file))
      (rl-history-load history-file))
    (while (not eof)
      (condition-case err
          (let ((line (funcall readline-function "EMACS> ")))
            (if line
                (if (string-match-p blank-or-comment-re line)
                    ;; treat empty line like C-c
                    (signal 'rl-cancel nil)
                  (when (not dumb-term-p)
                    (rl-history-add line))
                  (princ (format "%s\n" (rep line))))
              (princ "\n")
              (setq eof t)))
        (rl-cancel) ; proceed with new prompt
        (error
         (princ (format "%s\n" (error-message-string err))))))
    (when (not dumb-term-p)
      (rl-history-dump history-file))))

(repl)
