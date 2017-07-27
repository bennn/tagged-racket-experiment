#lang tagged/racket

(define-type C% (Class (f (-> Integer Integer))))

(define c% : C%
  (class object%
    (super-new)
    (define/public (f x) (+ x 1))))

(: g (-> (Vector (Instance C%)) Integer))
(define (g vo)
  (define o (vector-ref vo 0))
  (send o f (send o f 2)))

(g (vector (new c%)))
