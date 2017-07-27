#lang tagged/racket/base

(provide
  quad
  Quad quad?
  quad-attrs? QuadAttrs
  USQ)

(define-type USQ (U String Quad))

(: quad (-> Symbol QuadAttrs (Listof Any) Quad))
(define (quad name attrs items)
  (list* name attrs items))

;(define-type Val (U Integer String Symbol))
;; replace "Any" with this?

(define-type Quad (List* Symbol QuadAttrs (Listof Any)))
(define-predicate bgQuad? Quad)
(: Quad? (-> Any Boolean : Quad))
(define (Quad? x)
  (and (pair? x) (symbol? (car x))
       (pair? (cdr x)) (not (boolean? (cadr x))) (not (string? (cadr x))) (not (real? (cadr x))) (not (char? (cadr x)))
       (bgQuad? x)))
(define quad? Quad?)

(define-type QuadAttrs (Listof (Pairof Symbol Any)))
(define-predicate bgQuadAttrs? QuadAttrs)
(: QuadAttrs? (-> Any Boolean : QuadAttrs))
(define (QuadAttrs? x)
  (and (list? x) (andmap pair? x) (bgQuadAttrs? x)))
(define quad-attrs? QuadAttrs?)
