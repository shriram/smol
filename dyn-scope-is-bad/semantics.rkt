#lang racket

(require [for-syntax syntax/parse])

(require smol/hof/semantics)

(provide spy)
(provide [except-out [all-from-out smol/hof/semantics]
                     lambda λ
                     letrec
                     let
                     let*
                     set!
                     #%top
                     #%app

                     ; -- built-in functions --

                     ;; from smol/hof
                     map filter foldl foldr
                     empty list first rest empty?
                     cons

                     ;; from smol/state
                     mvec vec-set!
                     mpair set-left! set-right!

                     ;; from smol/fun
                     true false eq? equal?
                     ivec vec-len vec-ref
                     pair left right pair?
                     + - * /
                     zero?
                     = < <= > >=
                     ++ string=?
                     ])
(provide defvar deffun)
(provide [rename-out (dyn-λ λ)
                     (dyn-λ lambda)
                     (dyn-let let)
                     (dyn-let let*)
                     (dyn-let letrec)
                     (dyn-app #%app)
                     (dyn-set! set!)])

(define dvs (make-parameter (make-hasheq)))

(define (store name v)
  (hash-set! (dvs) name (box v)))

(define (internal-fetch name)
  (hash-ref (dvs) name
            (lambda ()
              (error name "undefined"))))

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
         (store 'var tmp))]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fname:id arg:id ...) body:expr ...+)
     #'(defvar fname
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
     #'(dyn-app (dyn-λ (var ...) body ...) val ...))))

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
     #'(update 'var val))))

(provide (rename-out [handle-id #%top]))

(define-syntax (handle-id stx)
  (syntax-case stx ()
    [(_ . var)
     (with-syntax ([stx stx])
       #'(fetch 'var))]))

(define-syntax (dyn-app stx)
  (syntax-parse stx
    [(_ fun:expr arg:expr ...)
     #'(parameterize ([dvs (hash-copy (dvs))])
         (#%app fun arg ...))]))

(define-syntax-rule (define-builtin* x ...)
  (begin
    (store 'x x)
    ...))

;; from smol/hof
(define-builtin* map filter foldl foldr)
(define-builtin* empty list first rest empty?)
(define-builtin* cons)

;; from smol/state
(define-builtin* mvec vec-set!)
(define-builtin* mpair set-left! set-right!)

;; from smol/fun
(define-builtin*
  true false eq? equal?
  ivec vec-len vec-ref
  pair left right pair?
  + - * /
  zero?
  = < <= > >=
  ++ string=?)
