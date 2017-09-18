#lang racket/base

(module t tagged/racket/base

  (provide (struct-out foo) yolo)

  (struct foo ((a : Natural)))

  (define (yolo (f : foo))
    (+ 1 (foo-a f))))

(require 't rackunit)

(check-pred values (foo 1))
(check-pred values (foo 'a))

(check-exn #rx"dynamic-typecheck"
  (λ () (yolo (foo 'a))))

(check-equal?
  (foo-a (foo 'a))
  'a)
