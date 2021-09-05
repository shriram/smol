#lang racket

(require
  [rename-in smol/fun/semantics
    [#%app checked-#%app]])

(provide
  [except-out [all-from-out smol/fun/semantics] checked-#%app]
  [rename-out [checked-#%app #%app]])

(provide mvec vset!)
(provide mpair set-left! set-right!)

(provide set!)

(define mvec vector)
(define vset! vector-set!)

(define (mpair a b)
  (mvec a b))
(define (set-left! p v)
  (unless (= (vlen p) 2)
    (error 'set-left! "the first argument must be a pair: ~a" p))
  (vector-set! p 0 v))
(define (set-right! p v)
  (unless (= (vlen p) 2)
    (error 'set-right! "the first argument must be a pair: ~a" p))
  (vector-set! p 1 v))
