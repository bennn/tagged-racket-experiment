#lang tagged/racket

(define-type QuadList
  (List (U String QuadList)))

(: flatten-attrs (-> (List (U #f QuadList)) Any))
(define (flatten-attrs attr)
  (if (car attr)
    ;(void)
    (car attr)
    (void)))
