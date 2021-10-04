#lang smol/dyn-scope-is-bad

(print-only-errors #t)

(test (map (λ (x) (* x x)) (list 1 2 4)) (list 1 4 16))

; -----

(deffun (hof-test-f g)
  (g 3))
(deffun (hof-test-h x)
  (+ x 1))

(test (hof-test-f hof-test-h) 4)

; -----

(defvar dx 0.0001)
(deffun (d/dx-1 f)
  ;; note: param named `p` to not clash with f's param
  (lambda (p)
    (/ (- (f (+ p dx)) (f p))
       dx)))

(defvar d-dx-sq-1 (d/dx-1 (lambda (x) (* x x))))
(defvar d-dx-sq-1@10 (d-dx-sq-1 10))
(test/pred d-dx-sq-1@10
           (λ (ans) (< 20 ans 21)))

(defvar d-dx-cb-1 (d/dx-1 (λ (x) (* x x x))))
(defvar d-dx-cb-1@10 (d-dx-cb-1 10))
(test/pred d-dx-cb-1@10
           (λ (ans) (< 300 ans 301)))
;; note: test/not
(test/not (d-dx-sq-1 10) d-dx-sq-1@10)

; - - -

(deffun (d/dx-2 f)
  (lambda (x)
    ;; param is now named x, same as f's param
    (/ (- (f (+ x dx)) (f x))
       dx)))

(defvar d-dx-sq-2 (d/dx-2 (lambda (x) (* x x))))
;; note: test/not (value comes out to be 0)
(test/not (d-dx-sq-2 10) d-dx-sq-1@10)

(defvar d-dx-cb-2 (d/dx-2 (λ (x) (* x x x))))
;; note: test/not (value comes out to be 0)
(test/not (d-dx-cb-2 10) d-dx-cb-1@10)

; -----

(test
 (let ([fact (λ (n)
               (if (zero? n)
                   1
                   (* n (fact (- n 1)))))])
   (fact 10))
 3628800)

(test
 (let ([even? (λ (n)
                (if (zero? n)
                    true
                    (odd? (- n 1))))]
       [odd? (λ (n)
               (if (zero? n)
                   false
                   (even? (- n 1))))])
   (even? 10))
 true)

; -----

(defvar to-set-1 0)
(set! to-set-1 5)
(test to-set-1 5)

((lambda () (set! to-set-1 6)))
(test to-set-1 6)

; -----

(test
 (let* ([x 1]
        [f (lambda () x)]
        [x 2])
   (f))
 2)
