#lang tagged/racket/base

(provide
  BoxQuad
  RunQuad
  SpacerQuad
  DocQuad
  Optical-KernQuad
  PieceQuad
  WordQuad
  Word-BreakQuad
  PageQuad
  Page-BreakQuad
  ColumnQuad
  Column-BreakQuad
  LineQuad
  BlockQuad
  Block-BreakQuad
  ;; --
  page-break?
  column-break?
  block-break?
  word-break?
  column?
  line?
  run?
  word?
  optical-kern?
  spacer?
)

;; -----------------------------------------------------------------------------

(require "../base/core-types.rkt")

;; =============================================================================

(define-type BoxQuad (List* 'box QuadAttrs QuadList))
(define-type RunQuad (List* 'run QuadAttrs QuadList))
(define-type SpacerQuad (List* 'spacer QuadAttrs QuadList))
(define-type DocQuad (List* 'doc QuadAttrs QuadList))
(define-type Optical-KernQuad (List* 'optical-kern QuadAttrs QuadList))
(define-type PieceQuad (List* 'piece QuadAttrs GroupQuadList))
(define-type WordQuad (List* 'word QuadAttrs QuadList))
(define-type Word-BreakQuad (List* 'word-break QuadAttrs QuadList))
(define-type PageQuad (List* 'page QuadAttrs GroupQuadList))
(define-type Page-BreakQuad (List* 'page-break QuadAttrs QuadList))
(define-type ColumnQuad (List* 'column QuadAttrs GroupQuadList))
(define-type Column-BreakQuad (List* 'column-break QuadAttrs QuadList))
(define-type LineQuad (List* 'line QuadAttrs GroupQuadList))
(define-type BlockQuad (List* 'block QuadAttrs QuadList))
(define-type Block-BreakQuad (List* 'block-break QuadAttrs QuadList))

(define-predicate bg:page-break? Page-BreakQuad)
(: page-break? (-> Any Boolean : Page-BreakQuad))
(define (page-break? x) (and (pair? x) (eq? 'page-break (car x)) (bg:page-break? x)))

(define-predicate bg:column-break? Column-BreakQuad)
(: column-break? (-> Any Boolean : Column-BreakQuad))
(define (column-break? x) (and (pair? x) (eq? 'column-break (car x)) (bg:column-break? x)))

(define-predicate bg:block-break? Block-BreakQuad)
(: block-break? (-> Any Boolean : Block-BreakQuad))
(define (block-break? x) (and (pair? x) (eq? 'block-break (car x)) (bg:block-break? x)))

(define-predicate bg:word-break? Word-BreakQuad)
(: word-break? (-> Any Boolean : Word-BreakQuad))
(define (word-break? x) (and (pair? x) (eq? 'word-break (car x)) (bg:word-break? x)))

(define-predicate bg:column? ColumnQuad)
(: column? (-> Any Boolean : ColumnQuad))
(define (column? x) (and (pair? x) (eq? 'column (car x)) (bg:column? x)))

(define-predicate bg:line? LineQuad)
(: line? (-> Any Boolean : LineQuad))
(define (line? x) (and (pair? x) (eq? 'line (car x)) (bg:line? x)))

(define-predicate bg:word? WordQuad)
(: word? (-> Any Boolean : WordQuad))
(define (word? x) (and (pair? x) (eq? 'word (car x)) (bg:word? x)))

(define-predicate bg:run? RunQuad)
(: run? (-> Any Boolean : RunQuad))
(define (run? x) (and (pair? x) (eq? 'run (car x)) (bg:run? x)))

(define-predicate bg:spacer? SpacerQuad)
(: spacer? (-> Any Boolean : SpacerQuad))
(define (spacer? x) (and (pair? x) (eq? 'spacer (car x)) (bg:spacer? x)))

(define-predicate bg:optical-kern? Optical-KernQuad)
(: optical-kern? (-> Any Boolean : Optical-KernQuad))
(define (optical-kern? x) (and (pair? x) (eq? 'optical-kern (car x)) (bg:optical-kern? x)))
