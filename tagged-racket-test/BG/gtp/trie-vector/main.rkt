#lang racket

(require "pfds-trie.rkt")

(define ITERS 100)

(define (main)
  (for/fold ([t (trie '((0)))])
            ([i (in-range ITERS)])
    (bind (list i) i t))
  (void))

(time (main))
