#lang smol/cc

;; Limited tests because we're simply re-exporting Racket's existing implementations

(print-only-errors #t)

(test (let/cc k (k 3)) 3)

(test (call/cc (λ (k) (k 3))) 3)
