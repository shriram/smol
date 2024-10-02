#lang racket

(require
  [rename-in smol/fun/semantics
    [#%app checked-#%app]])

(provide
  [except-out [all-from-out smol/fun/semantics] checked-#%app]
  [rename-out [checked-#%app #%app]])

(provide mvec vec-set!)
(provide mpair set-left! set-right!)

(provide set!)

(define mvec vector)
(define vec-set! vector-set!)

(define (mpair a b)
  (mvec a b))
(define (set-left! p v)
  (unless (= (vec-len p) 2)
    (error 'set-left! "the first argument must be a pair: ~a" p))
  (vector-set! p 0 v))
(define (set-right! p v)
  (unless (= (vec-len p) 2)
    (error 'set-right! "the first argument must be a pair: ~a" p))
  (vector-set! p 1 v))

(provide maybe?)
(define (maybe?)
  (>= (random) 0.5))
