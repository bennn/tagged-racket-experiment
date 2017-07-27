#lang racket/base

(require syntax/parse racket/match
         "../utils/utils.rkt"
         (types abbrev utils type-table)
         tagged-racket/defender/utils
         (for-syntax racket/base syntax/parse racket/syntax)
         (for-template racket/base racket/list racket/unsafe/ops))

(provide list-opt-expr)

(define-syntax (define-stxc stx)
  (syntax-parse stx
   [(_ mid (id* ...))
    #:with (id^* ...) (for/list ([id (in-list (syntax-e #'(id* ...)))])
                        (format-id id "~a^" (syntax-e id)))
    #'(begin
        (define-literal-syntax-class id*) ...
        (define-merged-syntax-class mid (id^* ...)))]))

(define-stxc ref [
  unbox
  car cdr mcar mcdr list-ref caar cadr cdar cddr caaar caadr cadar caddr cdaar
  cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr first rest second
  third fourth fifth sixth seventh eighth ninth tenth])

(define-syntax-class list-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  #:attributes (opt)
  (pattern (#%plain-app op:ref x*:opt-expr ...)
    #:with opt
    (dynamic-typecheck #'(#%plain-app op x*.opt ...) (type-of this-syntax))))
