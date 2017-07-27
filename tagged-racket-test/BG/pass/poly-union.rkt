#lang tagged/racket

;; Code from 'suffixtree',
;;  return a union of polymorphic variables.
;; Should have no runtime check
;;  (poly vars are fully checked statically)

(: f (All (A B) (-> (-> A) (-> B) (U A B))))
(define (f a b)
  (: helper (-> Integer (U A B)))
  (define (helper n)
    (if (zero? n) (a) (b)))
  (helper 0))

(f (lambda () #true) (lambda () 'false))
