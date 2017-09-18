#lang racket/base

;; Apply 3 kinds of typed functions:
;; - defined by TR
;; - require/typed'd
;; - blessed by stdlib (filter)

(module u racket/base
  (provide f)
  (define (f x)
    x))

(module t tagged/racket/base
  (require/typed (submod ".." u)
    (f (-> (Listof String) (Listof Natural))))

  (: g (-> Natural Boolean))
  (define (g x)
    (= x 4))

  (define (test)
    (filter g (f '("a" "bb" "ccc"))))

  (provide test))
(require 't rackunit)

(check-exn (λ (e) (or (regexp-match? #rx"expected: Natural" (exn-message e))
                      (regexp-match? #rx"dynamic-typecheck" (exn-message e))))
  test)
