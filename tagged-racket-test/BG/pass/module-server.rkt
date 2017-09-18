#lang tagged/racket/base

;; Provide a (higher-order) tagged function

(provide f filter)

(: f (-> Real (-> Real Real) Real))
(define (f r g)
  (g (g r)))
