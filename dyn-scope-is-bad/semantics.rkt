#lang racket

(require [for-syntax syntax/parse])

(require smol/hof/semantics)

(require [only-in mzscheme fluid-let])

(provide [except-out [all-from-out smol/hof/semantics]
                     ;defvar
                     ;deffun
                     lambda λ
                     letrec
                     let
                     let*
                     set!
                     #%top])
(provide defvar deffun)
(provide [rename-out (dyn-λ λ)
                     (dyn-λ lambda)
                     (dyn-let let)
                     (dyn-let letrec)
                     (dyn-let* let*)
                     (dyn-set! set!)])

(define dvs (make-hasheq))

(define (store name v)
  (hash-set! dvs name v))

(define (fetch name)
  (hash-ref dvs name
            (lambda ()
              (error name "undefined"))))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(let ([tmp rhs])
         (store 'var tmp))]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fname:id arg:id ...) body:expr ...+)
     #'(store 'fname
              (dyn-λ (arg ...) body ...))]))

(define-syntax (dyn-λ stx)
  (syntax-parse stx
    [(_ (arg:id ...) body:expr ...+)
     (with-syntax ([(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #'(lambda (tmp-arg ...)
           (store 'arg tmp-arg)
           ...
           body ...))]))

(define-syntax (dyn-let stx)
  (syntax-parse stx
    ([_ ([var:id val:expr] ...) body:expr ...+]
     (with-syntax ([(tmp ...)
                    (generate-temporaries #'(var ...))])
       #'(let ([tmp val] ...)
           (store 'var tmp)
           ...
           body ...)))))

(define-syntax dyn-let*
  (syntax-rules ()
    [(dyn-let* () body ...)
     (dyn-let () body ...)]
    [(dyn-let* (fst rest ...) body ...)
     (dyn-let (fst)
       (dyn-let* (rest ...) body ...))]))

(define-syntax (dyn-set! stx)
  (syntax-parse stx
    ([_ var:id val:expr]
     #'(store 'var val))))

(provide (rename-out [handle-id #%top]))

(define-syntax (handle-id stx)
  (syntax-case stx ()
    [(_ . any)
    (with-syntax ([stx stx])
      #'(fetch 'any))]))
