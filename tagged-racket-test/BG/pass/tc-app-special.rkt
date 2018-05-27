#lang tagged/racket/base

;; Test tc-app special cases for special things

(call-with-values (lambda () '42) (lambda ((n : Real)) (+ n 1)))
(call-with-values (lambda () (values 1 42)) (lambda ((n : Real) (m : Real)) (+ n m)))
