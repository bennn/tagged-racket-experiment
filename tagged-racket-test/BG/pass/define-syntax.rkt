#lang tagged/racket/base

(define-syntax-rule (yo lo)
  (+ lo lo))

(: f (-> Integer Integer))
(define (f x)
  (yo (yo x)))

(f 42)
