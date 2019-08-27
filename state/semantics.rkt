#lang racket

(require [for-syntax syntax/parse])

(require smol/fun/semantics)

(provide [all-from-out smol/fun/semantics])

(provide mvec vset!)
(provide mpair set-left! set-right!)

(provide set!)

(define mvec vector)
(define vset! vector-set!)

(define (mpair a b)
  (mvec a b))
(define (set-left! p v)
  (vector-set! p 0 v))
(define (set-right! p v)
  (vector-set! p 1 v))
