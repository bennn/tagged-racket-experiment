#lang racket/base
(require rackunit)

;; -----------------------------------------------------------------------------
;; basic contract generation for a function

(module a tagged/racket/base

  (provide f)

  (: f (-> Natural Natural))
  (define (f x) x)
)

(require 'a)

(f 4)
(check-exn exn:fail:contract?
  (lambda () (f "hello")))

(define g f)
(g 1)
(check-exn exn:fail:contract?
  (lambda () (g "hello")))

;; -----------------------------------------------------------------------------
;; test a non-function

(module b tagged/racket/base
  (provide n)
  (define n : Natural 4))

(require 'b)
n

;; -----------------------------------------------------------------------------
;; test a function with keyword

(module c tagged/racket/base

  (provide h)

  (: h (-> Natural #:extra Natural Natural))
  (define (h x #:extra y) y)
)

(require 'c)

(h 4 #:extra 5)
(h #:extra 5 4)
(check-exn exn:fail:contract?
  (lambda () (h 8)))

;; -----------------------------------------------------------------------------
;; test the cache, module exports the same type twice

(module d tagged/racket/base
  (provide i0 i1)
  (: i0 (-> Natural Natural))
  (: i1 (-> Natural Natural))
  (define (i0 n) (+ n n))
  (define (i1 n) (+ n n)))
(require 'd)

(+ (i0 1) (i1 2))

;; -----------------------------------------------------------------------------
;; test Any type

(module e tagged/racket/base
  (provide j)
  (: j (-> Any))
  (define (j) (box 3)))
(require 'e)

(let ([b (j)])
  (set-box! b 'A))

;; -----------------------------------------------------------------------------
;; test mandatory and optional keywords

(module f tagged/racket/base
  (provide k)
  (: k (->* (#:M Symbol) (#:O (U Symbol #false)) Void))
  (define (k #:M m #:O [o #f])
    (void)))
(module f2 tagged/racket/base
  (require/typed (submod ".." f)
    (k (->* [#:M Symbol] [#:O (U Symbol #false)] Void)))
  (k #:M 'a))
(require 'f 'f2)
(check-pred void? (k #:M 'a))
(check-pred void? (k #:M 'a #:O 'b))
