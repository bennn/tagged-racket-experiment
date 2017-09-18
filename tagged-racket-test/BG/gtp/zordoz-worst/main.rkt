#lang tagged/racket/base
(require require-typed-check/tagged)

(require/typed/check "zo-shell.rkt"
  [init (-> (Vectorof String) Void)])

;; Stress tests: search entire bytecode for the fairly-common branch struct
(define SELF-TEST '("base/zo-shell.zo" "base/zo-find.zo" "base/zo-string.zo" "base/zo-transition.zo"))
(define (self-test)
  (for ([b SELF-TEST]) (init (vector b "branch"))))

;; -----------------------------------------------------------------------------

(time (self-test))
