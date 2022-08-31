#lang racket

(require [for-syntax syntax/parse])

(require [only-in plai test test/pred test/exn print-only-errors equal~? error])

(require [only-in racket/trace trace untrace])
(provide [all-from-out racket/trace])

(provide #%module-begin #%top-interaction
	 #%datum #%top
	 require provide all-defined-out
         defvar deffun
         let let*
	 if and or not true false eq? equal?
	 begin
         ivec vec-len vec-ref
         pair left right pair?
	 + - * /
	 zero?
	 < <= > >=
	 ++ string=?
         spy)
(provide [except-out (all-from-out plai) equal~?]
	 [rename-out (my-app #%app)]
         test/not)
; provide all the `provide` sub-forms
; that seem to make sense
(provide all-defined-out all-from-out rename-out
         except-out prefix-out
         combine-out protect-out)

(define ++ string-append)

(define-syntax spy
  (lambda (exp)
    (syntax-case exp ()
      [(spy e)
       (with-syntax ([the-expr (syntax e)]
                     [the-src (syntax-source (syntax e))]
                     [the-line (syntax-line (syntax e))]
                     [the-col (syntax-column (syntax e))])
         (syntax
          (let ([val e])
            (begin
              (printf "Expression~n~a~nat ~a:~a:~a~nevaluated to~n~v~n~n"
                      (quote the-expr)
                      (quote the-src)
                      (quote the-line)
                      (quote the-col)
                      val)
              val))))])))

(define-syntax my-app
  (syntax-rules ()
    [(_ fun arg ...)
     (#%app fun
	    (let ([v arg])
	      (if (procedure? v)
		  (raise-syntax-error 'application "can't pass a function as an argument" #'arg)
		  v))
	    ...)]))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(define var rhs)]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fname:id arg:id ...) body:expr ...+)
     #'(define (fname arg ...) body ...)]))

(define-syntax test/not
  (syntax-rules ()
    [(_ L R)
     (test/pred L (lambda (v) (not (equal~? v R))))]))

(define ivec vector-immutable)
(define vec-len vector-length)
(define vec-ref vector-ref)

(define (pair a b)
  (ivec a b))
(define (left p)
  (unless (= (vec-len p) 2)
    (error 'left "argument must be a pair: ~a" p))
  (vec-ref p 0))
(define (right p)
  (unless (= (vec-len p) 2)
    (error 'right "argument must be a pair: ~a" p))
  (vec-ref p 1))
(define (pair? v) (and (vector? v) (= (vec-len v) 2)))
