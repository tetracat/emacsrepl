;; Copyright (c) 2016 Alain Kalker <a.c.kalker@gmail.com>
;; Based on work Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; http://bjoern.hoehrmann.de/utf-8/decoder/dfa/

(defconst utf8-accept 0)
(defconst utf8-reject 1)

(defconst utf8-decode [
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  ; 00..1f
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  ; 20..3f
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  ; 40..5f
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  ; 60..7f
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9  ; 80..9f
  7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7  ; a0..bf
  8 8 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2  ; c0..df
  #xa #x3 #x3 #x3 #x3 #x3 #x3 #x3 #x3 #x3 #x3 #x3 #x3 #x4 #x3 #x3  ; e0..ef
  #xb #x6 #x6 #x6 #x5 #x8 #x8 #x8 #x8 #x8 #x8 #x8 #x8 #x8 #x8 #x8  ; f0..ff
  #x0 #x1 #x2 #x3 #x5 #x8 #x7 #x1 #x1 #x1 #x4 #x6 #x1 #x1 #x1 #x1  ; s0..s0
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1  ; s1..s2
  1 2 1 1 1 1 1 2 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1  ; s3..s4
  1 2 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 3 1 1 1 1 1 1  ; s5..s6
  1 3 1 1 1 1 1 3 1 3 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ; s7..s8
  ])

(defun utf8-decode (state code-point byte)
  (let* ((type (elt utf8-decode byte))
         (code-point (if (not (= state utf8-accept))
                             (logior (logand byte #x3f) (lsh code-point 6))
                           (logand (lsh #xff (- type)) byte)))
         (state (elt utf8-decode (+ 256 (* state 16) type))))
    (cons state code-point)))
