#lang smol/hof

(print-only-errors #t)

(test (map (λ (x) (* x x)) (list 1 2 4)) (list 1 4 16))

(deffun (hof-test-f g)
  (g 3))
(deffun (hof-test-h x)
  (+ x 1))

(test (hof-test-f hof-test-h) 4)

(deffun (nested-i x)
  (deffun (nested-j y)
    (+ y 1))
  (nested-j (+ x 1)))

(test (nested-i 5) 7)

(test/pred (pair (lambda () 0) 0) (lambda (_) #t))
(test/pred (mpair (lambda () 0) 0) (lambda (_) #t))

(test/exn (cons 2 3) "list")

(test (letrec ([fact
                (lambda (n)
                  (if (zero? n)
                      1
                      (* n (fact (- n 1)))))])
        (fact 5))
      120)

(test
 (let* ([x 1]
        [f (lambda () x)]
        [x 2])
   (f))
 1)
