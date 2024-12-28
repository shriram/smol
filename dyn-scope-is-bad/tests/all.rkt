#lang smol/dyn-scope-is-bad

(print-only-errors #t)

(test (map (λ (x) (* x x)) (list 1 2 4)) (list 1 4 16))

; -----

(deffun (hof-test-f g)
  (g 3))
(deffun (hof-test-h x)
  (+ x 1))

(test (hof-test-f hof-test-h) 4)

; -----

(test
 (let ([fact (λ (n)
               (if (zero? n)
                   1
                   (* n (fact (- n 1)))))])
   (fact 10))
 3628800)

; -----

(test
 (let ([even? (λ (n)
                (if (zero? n)
                    true
                    (odd? (- n 1))))]
       [odd? (λ (n)
               (if (zero? n)
                   false
                   (even? (- n 1))))])
   (even? 10))
 true)

; -----

(defvar to-set-1 0)
(set! to-set-1 5)
(test to-set-1 5)

((lambda () (set! to-set-1 6)))
(test to-set-1 6)

; -----

(deffun (fibber x)
  (if (< x 2)
      (if (<= x 0) 0 1)
      (+ (fibber (- x 1)) (fibber (- x 2)))))

(test (fibber 5) 5)

; -----

(test/exn
 (let ((x 1))
   (+ (let ((f (lambda () x)))
        (let ((x 2) (y 3))
          (f)))
      y))
 "")

; -----

(test
 (let* ([x 1]
        [f (lambda () x)]
        [x 2])
   (f))
 2)

; -----
; shadow built-in name should be allowed

(test
  (let ([mvec 42])
     (eq? mvec 42))
  #t)