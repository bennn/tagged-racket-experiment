#lang racket/base

(require syntax/parse
         (for-template racket/base racket/unsafe/ops)
         "../utils/utils.rkt"
         (utils tc-utils)
         (types type-table struct-table)
         tagged-racket/defender/utils)

(provide struct-opt-expr)

(define struct-opt-msg "Struct access specialization.")

(define-syntax-class struct-op
  #:attributes (message opt idx)
  (pattern op:id
    #:when (struct-accessor? #'op)
    #:attr message "struct ref"
    #:with idx #`'#,(struct-fn-idx #'op)
    #:with opt #'unsafe-struct-ref)) ;;bg TODO probably should not optimize here, but rather just run the optimizer after

(define-syntax-class struct-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  ;; we can always optimize struct accessors and mutators
  ;; if they typecheck, they're safe
  (pattern (#%plain-app op:struct-op s:opt-expr v:opt-expr ...)
    #:with opt
    (dynamic-typecheck #'(op.opt s.opt op.idx v.opt ...) (type-of this-syntax))))
