#lang racket/base

(require smol/fun/semantics)

(provide [except-out (all-from-out smol/fun/semantics)
		     #%module-begin #%top-interaction
		     #%datum #%app #%top])

