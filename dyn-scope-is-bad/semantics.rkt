#lang racket

(require [for-syntax syntax/parse])

(require smol/hof/semantics)

(provide [except-out [all-from-out smol/hof/semantics]
                     ;defvar
                     ;deffun
                     lambda λ
                     letrec
                     let
                     set!
                     #%top
                     #%app])
(provide defvar deffun)
(provide [rename-out (dyn-λ λ)
                     (dyn-λ lambda)
                     (dyn-let let)
                     (dyn-let letrec)
                     (dyn-app #%app)
                     (dyn-set! set!)])

(define dvs (make-parameter (make-hasheq)))

(define (store name v)
  (hash-set! (dvs) (syntax->datum name) (box v)))

(define (internal-fetch name)
  (hash-ref (dvs) (syntax->datum name)
            (lambda ()
              (raise-syntax-error
               (syntax->datum name)
               "unbound identifier"
               name))))

(define (update name v)
  (define loc (internal-fetch name))
  (set-box! loc v))

(define (fetch name)
  (define loc (internal-fetch name))
  (unbox loc))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(let ([tmp rhs])
         (store #'var tmp))]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fname:id arg:id ...) body:expr ...+)
     #'(store #'fname
              (dyn-λ (arg ...) body ...))]))

(define-syntax (dyn-λ stx)
  (syntax-parse stx
    [(_ (arg:id ...) body:expr ...+)
     (with-syntax ([(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #'(lambda (tmp-arg ...)
           (store #'arg tmp-arg)
           ...
           body ...))]))

(define-syntax (dyn-let stx)
  (syntax-parse stx
    ([_ ([var:id val:expr] ...) body:expr ...+]
     #'(dyn-app (dyn-λ (var ...) body ...) val ...))))

(define-syntax (dyn-set! stx)
  (syntax-parse stx
    ([_ var:id val:expr]
     #'(update #'var val))))

(provide (rename-out [handle-id #%top]))

(define-syntax (handle-id stx)
  (syntax-case stx ()
    [(_ . any)
     (with-syntax ([stx stx])
       #'(fetch #'any))]))

(define-syntax (dyn-app stx)
  (syntax-parse stx
    [(_ fun:expr arg:expr ...)
     #'(parameterize ([dvs (hash-copy (dvs))])
         (#%app fun arg ...))]))
