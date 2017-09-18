#lang tagged/racket/base

;; Adding `begin` adds an "unnecessary" dynamic typecheck.

((begin +) 2 2)
