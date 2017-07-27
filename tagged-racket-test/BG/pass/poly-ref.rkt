#lang tagged/racket/base

;; Unpacking a value with polymorphic type should be a NO CHECK,
;;  because you can't do anything with it
;; (Type system guarantees you treat it as a black box, in this scope.)

(: f (All (A) (-> (Vectorof A) A)))
(define (f x)
  (vector-ref x 0))

(f (vector 1 2 3))
(f '#(four five six))
