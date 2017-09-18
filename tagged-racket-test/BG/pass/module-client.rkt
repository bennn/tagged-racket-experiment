#lang tagged/racket/base

;; Tagged module, imports function from another tagged module.
;;  Ideally, should not defend calls to the imported function.

(require "module-server.rkt")

(: h (-> Real Real))
(define (h n)
  (* n n))

(f 3 h)


(module u racket/base
  (provide xs)
  (define xs '(A B C)))
(require/typed 'u
  (xs (Listof Natural)))

(filter zero? xs)
