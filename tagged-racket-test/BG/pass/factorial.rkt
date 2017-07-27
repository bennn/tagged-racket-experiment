#lang tagged/racket

(: factorial (-> Natural Natural))
(define (factorial x)
  (for/product : Natural ([i : Natural (in-range x)])
    (+ i 1)))

(factorial 6)
