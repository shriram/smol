#lang racket

(require [except-in smol/state/semantics #%app])

(provide [all-from-out smol/state/semantics]
	 #%app)

(provide letrec)
(provide lambda λ)
(provide map filter foldl foldr)
(provide empty list first rest empty?)
(provide (rename-out [checked-cons cons]))

(define (checked-cons f r)
  (unless (list? r)
    (error 'cons "the second argument must be a list: ~a" r))
  (cons f r))
