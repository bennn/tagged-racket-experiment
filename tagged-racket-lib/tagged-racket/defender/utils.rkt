#lang racket/base

(require racket/match racket/sequence
         syntax/id-table racket/syntax syntax/stx
         syntax/parse
         syntax/parse/experimental/specialize
         racket/promise
         (for-syntax racket/base syntax/parse racket/syntax)
         "../utils/utils.rkt"
         (only-in (utils literal-syntax-class)
           [define-literal-syntax-class define-literal-syntax-class*])
         (for-template racket/base)
         (types type-table utils subtype match-expanders)
         (rep type-rep)
         (only-in syntax/srcloc build-source-location-list)
         (only-in tagged-racket/private/type-contract type->static-contract get-max-contract-kind)
         (only-in tagged-racket/static-contracts/combinators any/sc: function-combinator? with-new-name-defined-table with-new-name-tables procedure?/sc)
         (only-in tagged-racket/static-contracts/optimize optimize)
         (only-in tagged-racket/static-contracts/instantiate instantiate)
         (only-in tagged-racket/static-contracts/kinds contract-kind<=)
         (for-template (prefix-in c: racket/contract/base)))

(provide
         subtypeof? isoftype? dynamic-typecheck
         mk-unsafe-tbl
         n-ary->binary n-ary-comp->binary
         opt-expr defend
         value-expr typed-expr subtyped-expr
         kernel-expression
         define-unsafe-syntax-class
         define-literal-syntax-class
         define-merged-syntax-class
         syntax/loc/origin quasisyntax/loc/origin)

;; for tracking both origin and source location information
(define-syntax-rule (syntax/loc/origin loc op body)
  (syntax-track-origin (syntax/loc loc body) loc op))
(define-syntax-rule (quasisyntax/loc/origin loc op body)
  (syntax-track-origin (quasisyntax/loc loc body) loc op))

;; is the syntax object s's type a subtype of t?
(define (subtypeof? s t)
  (match (type-of s)
    [(tc-result1: (== t (lambda (x y) (subtype y x)))) #t] [_ #f]))
;; similar, but with type equality
(define (isoftype? s t)
  (match (type-of s)
         [(tc-result1: (== t)) #t] [_ #f]))

;; -----------------------------------------------------------------------------
;; bg BEGIN

(define-syntax (DEBUG stx)
  (if #false
    (syntax-case stx () [(_ . arg*) #'(printf . arg*)])
    #'(void)))

(define type->flat-contract
  (let ( #;[cache (make-hash)])
    (λ (t)
      (define (fail #:reason r)
        (raise-user-error 'dynamic-typecheck "failed to convert type ~a to flat contract because ~a" t r))
      (define ctc
        (cond
         #;[(hash-has-key? cache t)
          (hash-ref cache t)]
         [else
          (define sc (optimize (type->static-contract t fail)))
          (cond
           [(not (contract-kind<= (get-max-contract-kind sc) 'flat))
            (raise-arguments-error 'dynamic-typecheck "Internal Error: type generated non-flat static contract"
              "type" t
              "contract" sc)]
           [(any/sc? sc)
            #f]
           [else
            (define stx (with-new-name-defined-table (instantiate sc fail 'flat)))
            ;; TODO use #:cache
            ;; TODO don't reset name tables
            ;(hash-set! cache t stx)
            (DEBUG "TYPE->FLAT~n  type  ~a~n  sc    ~a~n  check ~a~n" t sc stx)
            stx])]))
      (and ctc
        #`(let () #,@(car ctc) (c:flat-contract-predicate #,(cadr ctc)))))))

(define (any/sc? sc)
  (match sc
   [(any/sc:)
    #true]
   [_
    #false]))

(define (need-to-check? t)
  (match t
   [(? Poly?)
    #f]
   [(? F?)
    ;; TODO are all type variables All-bound ??? (no way)
    #f]
   [_
    #t]))

;; ->type : Any -> Type
;; ... unclear, the input is just the result of `type-of`, nothing else
(define (->type tc)
  (match tc
   [(tc-result1: t)
    t]
   [_
    (raise-argument-error 'dynamic-typecheck "tc-results?" tc)]))

(define (dynamic-typecheck stx tc)
  (define t (->type tc))
  (if (need-to-check? t)
    (let ([ctc-stx (type->flat-contract t)]
          [errmsg (format "~e : ~a" (syntax->datum stx) t)])
      ;;bg wow, using contract is MUCH slower
      #;(c:contract #,ctc-stx #,stx 'tagged-world 'tagged-world "dynamic-typecheck" '#,srcloc)
      (if ctc-stx
        (quasisyntax/loc stx
          (let ([v #,stx])
            (if (#,ctc-stx v)
              v
              (error 'dynamic-typecheck '#,errmsg))))
        stx))
    stx))

;; --- END BG
;; -----------------------------------------------------------------------------

;; generates a table matching safe to unsafe promitives
(define (mk-unsafe-tbl generic safe-pattern unsafe-pattern)
  (for/fold ([h (make-immutable-free-id-table)]) ([g (in-list generic)])
    (let ([f (format-id g safe-pattern g)] [u (format-id g unsafe-pattern g)])
      (free-id-table-set (free-id-table-set h g u) f u))))

;; unlike their safe counterparts, unsafe binary operators can only take 2 arguments
;; this works on operations that are (A A -> A)
(define (n-ary->binary src-stx op stx)
  (for/fold ([o (stx-car stx)]) ([e (in-syntax (stx-cdr stx))])
    (quasisyntax/loc src-stx
      (#,op #,o #,e))))
;; this works on operations that are (A A -> B)
(define (n-ary-comp->binary src-stx op arg1 arg2 rest)
  ;; First, generate temps to bind the result of each arg2 args ...
  ;; to avoid computing them multiple times.
  (define lifted (stx-map (lambda (x) (generate-temporary)) #`(#,arg2 #,@rest)))
  ;; Second, build the list ((op arg1 tmp2) (op tmp2 tmp3) ...)
  (define tests
    (let loop ([res  (list #`(#,op #,arg1 #,(car lifted)))]
               [prev (car lifted)]
               [l    (cdr lifted)])
      (cond [(null? l) (reverse res)]
            [else (loop (cons #`(#,op #,prev #,(car l)) res)
                        (car l)
                        (cdr l))])))
  ;; Finally, build the whole thing.
  (quasisyntax/loc src-stx
    (let #,(for/list ([lhs (in-list lifted)]
                      [rhs (in-syntax #`(#,arg2 #,@rest))])
             #`(#,lhs #,rhs))
      (and #,@tests))))

; to avoid mutually recursive syntax classes
; will be set to the actual defend function at the entry point
(define defend (make-parameter #f))

(define-syntax-class opt-expr
  #:commit
  #:attributes (opt)
  (pattern e:expr #:attr opt (delay ((defend) #'e))))


(define-syntax (define-unsafe-syntax-class stx)
  (define-splicing-syntax-class spec
    #:attributes (class-name (literals 1) unsafe-id)
    (pattern (~seq class-name:id (literals:id ...) unsafe-id:id))
    (pattern literal:id
      #:with (literals ...) #'(literal)
      #:with class-name (format-id #'literal "~a^" #'literal)
      #:with unsafe-id (format-id #'literal "unsafe-~a" #'literal)))
  (syntax-parse stx
    [(_ :spec)
     #'(begin
         (define-literal-syntax-class literal (literals ...))
         (define-syntax-class class-name
           (pattern :literal #:with unsafe #'unsafe-id)))]))

(define-syntax (define-literal-syntax-class stx)
  (syntax-parse stx
    [(_ . args)
     #'(define-literal-syntax-class* #:for-template . args)]))

(define-syntax-rule (define-merged-syntax-class name (syntax-classes ...))
  (define-syntax-class name
    #:auto-nested-attributes
    (pattern (~var || syntax-classes)) ...))

(define-syntax-class (typed-expr predicate)
  #:attributes (opt)
  (pattern (~and e :opt-expr)
           #:when (match (type-of #'e)
                    [(tc-result1: (? predicate)) #t]
                    [_ #f])))

(define-syntax-class/specialize (subtyped-expr type)
  (typed-expr (λ (t) (subtype t type))))

(define-syntax-class value-expr
  #:attributes (val opt)
  #:literal-sets (kernel-literals)
  (pattern (quote v)
    #:attr val (syntax-e #'v)
    #:with opt this-syntax)
  (pattern (~and e :opt-expr)
    #:when (match (type-of #'e)
             [(tc-result1: (Val-able: _))
              #t]
             [_ #f])
    #:attr val (match (type-of #'e)
                 [(tc-result1: (Val-able: v)) v]
                 [_ #f])))

(define-syntax-class kernel-expression
  #:commit
  #:literal-sets (kernel-literals)
  #:attributes [(sub-exprs 1)]
  [pattern (begin sub-exprs:expr ...)]
  [pattern ((~or begin0 #%plain-app) sub-exprs:expr ...+)]
  [pattern (#%plain-lambda formals sub-exprs:expr ...)]
  [pattern ((~or if with-continuation-mark) e1:expr e2:expr e3:expr)
    #:with (sub-exprs ...) #'(e1 e2 e3)]
  [pattern (~or (#%top . _) (#%variable-reference . _) (quote _) (quote-syntax . _) :id)
    #:with (sub-exprs ...) #'()]
  [pattern (case-lambda [formals e*:expr ...] ...)
    #:with (sub-exprs ...) #'(e* ... ...)]
  [pattern ((~or let-values letrec-values) ([ids e-rhs:expr] ...) e-body:expr ...)
    #:with (sub-exprs ...) #'(e-rhs ... e-body ...)]
  [pattern (#%expression e:expr)
    #:with (sub-exprs ...) #'(e)]
  [pattern (set! _ e:expr)
    #:with (sub-exprs ...) #'(e)])
