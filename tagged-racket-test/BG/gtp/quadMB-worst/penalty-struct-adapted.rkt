#lang tagged/racket/base
(require require-typed-check/tagged)

(provide (struct-out $penalty))

;; -----------------------------------------------------------------------------

(require
  "base/core-types.rkt")

(require/typed/check "penalty-struct.rkt"
  [#:struct $penalty ([hyphens : Nonnegative-Integer]
                      [width : Value-Type])])

;; =============================================================================

(define-type Value-Type Float)
