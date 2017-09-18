#lang racket/base
(require rackunit)

(module u racket/base
  (provide f)
  (define (f b)
    ((vector-ref b 0) 2)))

(module t tagged/racket/base
  (provide test1 test0)
  (require/typed (submod ".." u)
    (f (-> (Vector (-> String String)) Integer)))

  (: q (-> String String))
  (define (q n)
    (string-append n n))

  (define (test0)
    (f (vector q)))

  (define b (vector q))
  (define (test1)
    (f b)))
(require 't)

(check-exn #rx"tagged-world"
  test0)

(check-exn #rx"tagged-world"
  test1)
