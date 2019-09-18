#lang racket/base

(require smol/dyn-scope-is-bad/semantics)

(provide [except-out (all-from-out smol/dyn-scope-is-bad/semantics)
		     #%module-begin #%top-interaction
		     #%datum #%app])

