#lang tagged/racket/base

;; `(apply f ...)` should not generate tag-check when `f` is tag-safe

(void (apply string-append '("one" "two" "three")))
