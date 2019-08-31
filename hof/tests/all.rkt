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
