#lang racket

(require [except-in smol/state/semantics #%app])

(provide [all-from-out smol/state/semantics]
	 #%app)

(provide lambda λ)
(provide map filter foldl foldr)
(provide cons empty list)
