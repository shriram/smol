#lang smol/state

(print-only-errors #t)

(defvar p (mpair 'a 'b))
(test (left p) 'a)
(test (right p) 'b)

(set-left! p 'x)
(set-right! p 'y)
(test (left p) 'x)
(test (right p) 'y)

(test (vlen p) 2)
(test (pair? p) true)
(test (pair? (mvec 'g 'h)) true)

(defvar v 2)
(test v 2)
(set! v 3)
(test v 3)

(defvar y (pair 1 2))
(defvar z y)
(test y (pair 1 2))
(test z (pair 1 2))
(set! y (pair 3 4))
(test y (pair 3 4))
(test z (pair 1 2))

(defvar w (mvec 1 2 3))
(vset! w 2 4)
(test w '#(1 2 4))

(deffun (nested-i x)
  (deffun (nested-j y)
    (+ y 1))
  (nested-j (+ x 1)))

(test (nested-i 5) 7)

(test/exn (set-left! (ivec 1 2 3) 0) "pair")
(test/exn (set-right! (ivec 1 2 3) 0) "pair")