#lang racket/base

(module util racket/base
  (provide random-from)
  (require racket/list)

  (define (random-from xs)
    (first (shuffle xs))))

(module client tagged/racket/base
  (require/typed (submod ".." util)
    (random-from (All (A) (-> (Listof A) A))))

  (random-from '(1 2 3)))
(require 'client)
