#lang smol/fun

(provide [all-defined-out])

(print-only-errors #t)

(deffun (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

(test (fact 10) 3628800)

(deffun (odd? n)
  (if (zero? n)
      false
      (even? (- n 1))))

(deffun (even? n)
  (if (zero? n)
      true
      (odd? (- n 1))))

(test (odd? 10) false)
(test (even? 10) true)

(defvar x 3)

(deffun (f x) (+ x x))

(test (f 5) 10)

(test/not (let ([x 8] [y x]) x) 3)

(defvar l (pair 'a (pair 'b (pair 'c 0))))
(test (equal? l '#(a #(b #(c 0)))) true)
(test (eq? l '#(a #(b #(c 0)))) false)

(deffun (len l)
  (if (not (pair? l))
      0
      (+ 1 (len (right l)))))
(test (len 0) 0)
(test (len l) 3)

(deffun (nested-i x)
  (deffun (nested-j y)
    (+ y 1))
  (nested-j (+ x 1)))

(test (nested-i 5) 7)