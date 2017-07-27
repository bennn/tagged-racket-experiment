#lang racket/base
(provide struct-predicate-procedure?/c)
(require (only-in ffi/unsafe cpointer-predicate-procedure?))

(define (struct-predicate-procedure?/c x)
  (and (or (struct-predicate-procedure? x)
           (cpointer-predicate-procedure? x))
       (not (impersonator? x))))
