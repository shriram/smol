#lang smol/hof

(print-only-errors #t)

(test (map (λ (x) (* x x)) (list 1 2 4)) (list 1 4 16))
