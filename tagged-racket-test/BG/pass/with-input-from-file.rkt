#lang tagged/racket/base

;; Make sure 'with-input-from-file' works

(: main (-> Path-String Void))
(define (main filename)
  (define q (with-input-from-file filename (lambda () (read))))
  (void))
