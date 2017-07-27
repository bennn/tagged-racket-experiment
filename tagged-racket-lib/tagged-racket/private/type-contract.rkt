#lang racket/base

;; Contract generation for Typed Racket

(require
 "../utils/utils.rkt"
 syntax/parse
 (rep type-rep prop-rep object-rep fme-utils)
 (utils tc-utils)
 (env type-name-env row-constraint-env)
 (rep core-rep rep-utils type-mask values-rep base-types numeric-base-types)
 (types resolve utils printer match-expanders union)
 (prefix-in t: (types abbrev numeric-tower subtype))
 (private parse-type syntax-properties)
 racket/match racket/syntax racket/list
 racket/format
 syntax/flatten-begin
 (only-in (types abbrev) -Bottom -Boolean)
 (static-contracts instantiate optimize structures combinators constraints)
 (only-in (submod tagged-racket/static-contracts/instantiate internals) compute-constraints)
 ;; TODO make this from contract-req
 (prefix-in c: racket/contract)
 (contract-req)
 (for-syntax racket/base)
 (for-template racket/base racket/contract (only-in racket/function arity-includes?)))

(provide
  (c:contract-out
    [type->static-contract
      (c:parametric->/c (a) ((Type? (c:-> #:reason (c:or/c #f string?) a))
                             (#:typed-side boolean?) . c:->* . (c:or/c a static-contract?)))]))

(provide change-contract-fixups
         change-provide-fixups
         extra-requires
         include-extra-requires?
         get-max-contract-kind) ;;bg

;; submod for testing
(module* test-exports #f (provide type->contract))

;; has-contrat-def-property? : Syntax -> Boolean
(define (has-contract-def-property? stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(define-values (_) e)
     (and (contract-def-property #'e)
          #t)]
    [_ #f]))

(struct contract-def (type flat? maker? typed-side) #:prefab)

;; get-contract-def-property : Syntax -> (U False Contract-Def)
;; Checks if the given syntax needs to be fixed up for contract generation
;; and if yes it returns the information stored in the property
(define (get-contract-def-property stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(define-values (_) e)
     (and (contract-def-property #'e)
          ((contract-def-property #'e)))]
    [_ #f]))

;; type->contract-fail : Syntax Type #:ctc-str String
;;                       -> #:reason (Option String) -> Void
;; Curried function that produces a function to report
;; type->contract failures
(define ((type->contract-fail to-check to-report
                              #:ctc-str [ctc-str "contract"])
         #:reason [reason #f])
  (tc-error/stx
   to-report
   (~a "Type ~a could not be converted to a "
       ctc-str
       (if reason (~a ": " reason) "."))
   to-check))

;; The cache/sc-cache are used to share contract and static contract
;; definitions respectively across multiple calls to type->contract.
;; This saves computation time and zo space for excessively large types
;; (such as mutually recursive class types).
(define (generate-contract-def stx cache sc-cache)
  (define prop (get-contract-def-property stx))
  (match-define (contract-def type-stx flat? maker? typed-side) prop)
  (define *typ (if type-stx (parse-type type-stx) t:-Dead-Code))
  (define kind (if (and type-stx flat?) 'flat 'impersonator))
  (syntax-parse stx #:literals (define-values)
    [(define-values (n) _)
     (define typ
       (cond [maker?
              (match (lookup-type-name (Name-id *typ))
                [(Poly-names: names body)
                 (make-Poly names
                   ((map fld-t (Struct-flds body)) #f . t:->* . *typ))]
                [ty
                 ((map fld-t (Struct-flds ty)) #f . t:->* . *typ)])]
             [else *typ]))
     (match-define (list defs ctc)
       (type->contract
        typ
        ;; this value is from the typed side (require/typed, make-predicate, etc)
        ;; unless it's used for with-type
        #:typed-side (from-typed? typed-side)
        #:kind kind
        #:cache cache
        #:sc-cache sc-cache
        (type->contract-fail
         typ type-stx
         #:ctc-str (if flat? "predicate" "contract"))))
     (ignore ; should be ignored by the optimizer
      (quasisyntax/loc stx
        (begin #,@defs (define-values (n) #,ctc))))]
    [_ (int-err "should never happen - not a define-values: ~a"
                (syntax->datum stx))]))

(define TAGGED-USE-CHAPERONE? #false)

(define (generate-contract-def/provide stx cache sc-cache)
  (match-define (list type untyped-id orig-id blame-id)
                (contract-def/provide-property stx))
  (define failure-reason #f)
  (define is-function? (is-a-function-type? type))
  (define fail-thunk (λ (#:reason [reason #f]) (set! failure-reason reason)))
  (define result
    ;;bg: `result` is a "normal" contract ... either chaperone or tag-check
    (cond
     [is-function?
      (define f (type->function-type type))
      (type->function-chaperone-contract f fail-thunk #:cache cache #:sc-cache sc-cache)]
     [else
      (type->contract type
                      #:typed-side #t
                      #:kind 'impersonator
                      #:cache cache
                      #:sc-cache sc-cache
                      ;; FIXME: get rid of this interface, make it functional
                      (λ (#:reason [reason #f]) (set! failure-reason reason)))]))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(define-values (ctc-id) _)
     ;; no need for ignore, the optimizer doesn't run on this code
     (cond
      [failure-reason
       #`(define-syntax (#,untyped-id stx)
           (tc-error/fields #:stx stx
                            "could not convert type to a contract"
                            #:more #,failure-reason
                            "identifier" #,(symbol->string (syntax-e orig-id))
                            "type" #,(pretty-format-rep type #:indent 8)))]
      [(and is-function? TAGGED-USE-CHAPERONE?)
       (match-define (list defs ctc) result)
  (DEBUG "module-boundary~n  type is ~a~n  contract is ~a~n" type (syntax->datum ctc))
       (define maybe-inline-val
         (should-inline-contract?/cache ctc cache))
       #`(begin #,@defs
                #,@(if maybe-inline-val
                       null
                       (list #`(define-values (ctc-id) #,ctc)))
                (define-module-boundary-contract #,untyped-id
                  #,orig-id
                  #,(or maybe-inline-val #'ctc-id)
                  #:pos-source #,blame-id
                  #:srcloc (vector (quote #,(syntax-source orig-id))
                                   #,(syntax-line orig-id)
                                   #,(syntax-column orig-id)
                                   #,(syntax-position orig-id)
                                   #,(syntax-span orig-id))))]
      [is-function?
       ;; try to inline checks at function calls
       (match-define (list defs ctc) result)
       (define maybe-inline-val
         (should-inline-contract?/cache ctc cache))
       (define ctc-map (function-type->domain-contract-map (type->function-type type) fail-thunk))
       (define chap-id (gensym (syntax-e untyped-id)))
       (define map-id (gensym (syntax-e untyped-id)))
       (define args->arity-key-id (gensym 'args->arity-key))
       #`(begin #,@defs
                #,@(if maybe-inline-val
                       null
                       (list #`(define-values (ctc-id) #,ctc)))
                (define-module-boundary-contract #,chap-id
                  #,orig-id
                  #,(or maybe-inline-val #'ctc-id)
                  #:pos-source #,blame-id
                  #:srcloc (vector (quote #,(syntax-source orig-id))
                                   #,(syntax-line orig-id)
                                   #,(syntax-column orig-id)
                                   #,(syntax-position orig-id)
                                   #,(syntax-span orig-id)))
                (define-for-syntax #,map-id
                  (map (lambda (x) (cons (syntax-e (car x)) (syntax-e (cdr x))))
                       (map syntax-e (syntax-e #'#,ctc-map))))
                (define-for-syntax (#,args->arity-key-id args)
                  (let loop ([args args] [acc (cons 0 '())])
                    (cond
                     [(null? args)
                      (cons (car acc) (sort (cdr acc) keyword<?))]
                     [(keyword? (car args))
                      (define acc+ (cons (car acc)
                                         (cons (syntax-e (car args)) (cdr acc)))) ;; TODO `insert` instead of sorting at end
                      (loop (cddr args) acc+)]
                     [else
                      (define acc+ (cons (+ 1 (car acc)) (cdr acc)))
                      (loop (cdr args) acc+)])))
                (define-syntax (#,untyped-id stx)
                  (if #false #;(unbox tagged-context?)
                    #'#,orig-id
                    (syntax-parse stx
                     [(_ . ?args)
                      #:with errmsg (format "~e" (syntax->datum stx))
                      #:with wrapped-args
                        (let* ([args (syntax-e #'?args)]
                               [arity-key (#,args->arity-key-id args)]
                               [key->ctc (cdr (or (assoc arity-key #,map-id)
                                                  (assoc #f #,map-id)
                                                  (cons #f #f)))])
                          (if (not key->ctc)
                            args ;; should error at runtime
                            (let loop ([args args]
                                       [i 0])
                              (cond
                               [(null? args)
                                '()]
                               [(keyword? (syntax-e (car args)))
                                (define ctc (hash-ref key->ctc (syntax-e (car args)) #'void))
                                (list* (car args)
                                       #`(let ([v #,(cadr args)])
                                           (if (#,ctc v) v (raise-arguments-error 'dynamic-typecheck 'errmsg)))
                                       (loop (cddr args) i))]
                               [else ;; positional or rest-arg
                                (define ctc
                                  (or (hash-ref key->ctc i #f)
                                      (hash-ref key->ctc 'rest #f)
                                      #'values))
                                (cons #`(let ([v #,(car args)])
                                          (if (#,ctc v) v (raise-arguments-error 'dynamic-typecheck 'errmsg)))
                                      (loop (cdr args) (+ i 1)))]))))
                      #'(#,orig-id . wrapped-args)]
                     [:id
                      #'#,chap-id]))))]
      [else
       ;; original TR code
       (match-define (list defs ctc) result)
       (define maybe-inline-val
         (should-inline-contract?/cache ctc cache))
       #`(begin #,@defs
                #,@(if maybe-inline-val
                       null
                       (list #`(define-values (ctc-id) #,ctc)))
                (define-module-boundary-contract #,untyped-id
                  #,orig-id
                  #,(or maybe-inline-val #'ctc-id)
                  #:pos-source #,blame-id
                  #:srcloc (vector (quote #,(syntax-source orig-id))
                                   #,(syntax-line orig-id)
                                   #,(syntax-column orig-id)
                                   #,(syntax-position orig-id)
                                   #,(syntax-span orig-id))))])]))

;; Syntax (Dict Static-Contract (Cons Id Syntax)) -> (Option Syntax)
;; A helper for generate-contract-def/provide that helps inline contract
;; expressions when needed to cooperate with the contract system's optimizations
(define (should-inline-contract?/cache ctc-stx cache)
  (and (identifier? ctc-stx)
       (let ([match? (assoc ctc-stx (hash-values cache) free-identifier=?)])
         (and match?
              (should-inline-contract? (cdr match?))
              (cdr match?)))))

;; The below requires are needed since they provide identifiers that
;; may appear in the residual program.

;; TODO: It would be better to have individual contracts specify which
;; modules should be required, but for now this is just all of them.
(define extra-requires
  #'(require
      (submod tagged-racket/private/type-contract predicates)
      tagged-racket/utils/utils
      (for-syntax tagged-racket/utils/utils)
      tagged-racket/utils/any-wrap
      tagged-racket/utils/struct-type-c
      tagged-racket/utils/opaque-object
      tagged-racket/utils/evt-contract
      tagged-racket/utils/sealing-contract
      tagged-racket/utils/promise-not-name-contract
      tagged-racket/utils/simple-result-arrow
      racket/sequence
      racket/contract/parametric))

;; Should the above requires be included in the output?
;;   This box is only used for contracts generated for `require/typed`
;;   and `cast`, contracts for `provides go into the `#%contract-defs`
;;   submodule, which always has the above `require`s.
;;bg: yes always, for now
(define include-extra-requires? (box #t))

(define (change-contract-fixups forms)
  (define ctc-cache (make-hash))
  (define sc-cache (make-hash))
  (with-new-name-tables
   (for/list ((e (in-list forms)))
     (if (not (has-contract-def-property? e))
         e
         (begin (set-box! include-extra-requires? #t)
                (generate-contract-def e ctc-cache sc-cache))))))

;; TODO: These are probably all in a specific place, which could avoid
;;       the big traversal
(define (change-provide-fixups forms  [ctc-cache (make-hash)] [sc-cache (make-hash)])
  (with-new-name-tables
   (for/list ([form (in-list forms)])
     (syntax-parse form #:literal-sets (kernel-literals)
       [_
        #:when (contract-def/provide-property form)
        (generate-contract-def/provide form ctc-cache sc-cache)]
       [(module* name #f forms ...)
        (quasisyntax/loc form
          (module* name #f 
            #,@(change-provide-fixups (syntax->list #'(forms ...))
                                      ctc-cache sc-cache)))]
       [((~literal #%plain-module-begin) forms ...)
        (quasisyntax/loc form
          (#%plain-module-begin
           #,@(change-provide-fixups (flatten-all-begins #'(begin forms ...))
                                     ctc-cache sc-cache)))]
       [_ form]))))

;; get-max-contract-kind
;; static-contract -> (or/c 'flat 'chaperone 'impersonator)
;; recurse into a contract finding the max
;; kind (e.g. flat < chaperone < impersonator)
(define (get-max-contract-kind sc)
  (kind-max-max (contract-restrict-value (compute-constraints sc 'impersonator))))

;; To avoid misspellings
(define impersonator-sym 'impersonator)
(define chaperone-sym 'chaperone)
(define flat-sym 'flat)

(define (contract-kind-max i . args)
  (define (contract-kind-max2 x y)
    (cond
      ((equal? flat-sym x) y)
      ((equal? flat-sym y) x)
      ((equal? chaperone-sym x) y)
      ((equal? chaperone-sym y) x)
      (else impersonator-sym)))
  (for/fold ((acc i)) ((v (in-list args)))
    (contract-kind-max2 v acc)))

(define (contract-kind-min i . args)
  (define (contract-kind-min2 x y)
    (cond
      ((equal? flat-sym x) x)
      ((equal? flat-sym y) y)
      ((equal? chaperone-sym x) x)
      ((equal? chaperone-sym y) y)
      (else impersonator-sym)))
  (for/fold ((acc i)) ((v (in-list args)))
    (contract-kind-min2 v acc)))


(define (contract-kind->keyword sym)
  (string->keyword (symbol->string sym)))

(define (from-typed? side)
  (case side
   [(typed both) #t]
   [(untyped) #f]))

(define (from-untyped? side)
  (case side
   [(untyped both) #t]
   [(typed) #f]))

(define (flip-side side)
  (case side
   [(typed) 'untyped]
   [(untyped) 'typed]
   [(both) 'both]))

;; type->contract : Type Procedure
;;                  #:typed-side Boolean #:kind Symbol #:cache Hash
;;                  -> (U Any (List (Listof Syntax) Syntax))
(define (type->contract ty init-fail
                        #:typed-side [typed-side #t]
                        #:kind [kind 'impersonator]
                        #:cache [cache (make-hash)]
                        #:sc-cache [sc-cache (make-hash)])
  (let/ec escape
    (define (fail #:reason [reason #f]) (escape (init-fail #:reason reason)))
    (instantiate
     (optimize
      (type->static-contract ty #:typed-side typed-side fail
                             #:cache sc-cache)
      #:trusted-positive typed-side
      #:trusted-negative (not typed-side))
     fail
     kind
     #:cache cache)))

(define (positional-args/sc base-arity rst?)
  (flat/sc #`(λ (g)
               (arity-includes?
                 (procedure-arity g)
                 #,(if rst? #`(arity-at-least '#,base-arity) #`'#,base-arity)))))

(define (keyword-args/sc req* opt*)
  (flat/sc
    #`(λ (g)
        (define-values [f-req-k* f-opt-k*] (values '#,req* '#,opt*))
        (define-values [g-req-k* g-opt-k*] (procedure-keywords g))
        (and (equal? f-req-k* g-req-k*)
             ;; f optional keywords are a subset of g optional keywords
             (or (not f-opt-k*)
                 (and g-opt-k*
                      (let loop ([f* f-opt-k*] [g* g-opt-k*])
                        (cond
                         [(null? f*)
                          #true]
                         [(or (null? g*) (keyword<? (car g*) (car f*)))
                          #false]
                         [(keyword<? (car f*) (car g*))
                          (loop f* (cdr g*))]
                         [else
                          (loop (cdr f*) (cdr g*))]))))))))

(define (t->sc/function f fail typed-side recursive-values loop method?)
  (match-define (Fun: arrs) f)
  (apply or/sc
    (for/list ([a (in-list arrs)])
      (t->sc/arrow a fail typed-side recursive-values loop method?))))

(define (t->sc/arrow arrow fail typed-side recursive-values loop method?)
  (match-define (Arrow: dom rst kws _) arrow)
  (define base-arity (+ (length dom) (if method? 1 0)))
  (define f-req-k*
    (for/list ([k (in-list kws)]
               #:when (Keyword-required? k))
      (Keyword-kw k)))
  (define f-opt-k*
    (map Keyword-kw kws))
  (and/sc
    (positional-args/sc base-arity (and rst #t))
    (keyword-args/sc f-req-k* f-opt-k*)))

(struct triple (untyped typed both))
(define (triple-lookup trip side)
  (case side
    ((untyped) (triple-untyped trip))
    ((typed) (triple-typed trip))
    ((both) (triple-both trip))))
(define (same sc)
  (triple sc sc sc))

;; Macro to simplify (and avoid reindentation) of the match below
;;
;; The sc-cache hashtable is used to memoize static contracts. The keys are
;; a pair of the Type-seq number for a type and 'untyped or 'typed
(define-syntax (cached-match stx)
  (syntax-case stx ()
    [(_ sc-cache type-expr typed-side-expr match-clause ...)
     #'(let ([type type-expr]
             [typed-side typed-side-expr])
         (define key (cons type typed-side))
         (cond [(hash-ref sc-cache key #f)]
               [else
                (define sc (match type match-clause ...))
                (define fvs (fv type))
                ;; Only cache closed terms, otherwise open terms may show up
                ;; out of context.
                (unless (or (not (null? fv))
                            ;; Don't cache types with applications of Name types because
                            ;; it does the wrong thing for recursive references
                            (has-name-app? type))
                  (hash-set! sc-cache key sc))
                sc]))]))

(define-syntax (DEBUG stx)
  (if #false
    (syntax-case stx () ((_ msg . args) #'(printf msg . args)))
    #'(void)))

(define (type->static-contract type init-fail
                               #:typed-side [typed-side #t]
                               #:cache [sc-cache (make-hash)]
                               #:init-recursive-values [rv0 (hash)])
  (let/ec return
    (define (fail #:reason reason) (return (init-fail #:reason reason)))
    (let loop ([type type] [typed-side (if typed-side 'typed 'untyped)] [recursive-values rv0])
      (define (t->sc t #:recursive-values (recursive-values recursive-values))
        (loop t typed-side recursive-values))
      (define (t->sc/neg t #:recursive-values (recursive-values recursive-values))
        (loop t (flip-side typed-side) recursive-values))
      (define (t->sc/both t #:recursive-values (recursive-values recursive-values))
        (loop t 'both recursive-values))
      (define (t->sc/meth t)
        (t->sc/method t fail typed-side recursive-values loop))
      (define (t->sc/fun t)
        (t->sc/function t fail typed-side recursive-values loop #f))

      (define (prop->sc p)
        (match p
          [(TypeProp: o (app t->sc tc))
           (cond
             [(not (equal? flat-sym (get-max-contract-kind tc)))
              (fail #:reason "proposition contract generation not supported for non-flat types")]
             [else (is-flat-type/sc (obj->sc o) tc)])]
          [(NotTypeProp: o (app t->sc tc))
           (cond
             [(not (equal? flat-sym (get-max-contract-kind tc)))
              (fail #:reason "proposition contract generation not supported for non-flat types")]
             [else (not-flat-type/sc (obj->sc o) tc)])]
          [(LeqProp: (app obj->sc lhs) (app obj->sc rhs))
           (leq/sc lhs rhs)]
          ;; TODO: check for (<= x y) and (<= y x)
          ;; and generate and = instead of two <=
          [(AndProp: ps)
           (and-prop/sc (map prop->sc ps))]
          [(OrProp: ps)
           (or-prop/sc (map prop->sc ps))]))

      (define (obj->sc o)
        (match o
          [(Path: pes (? identifier? x))
           (for/fold ([obj (id/sc x)])
                     ([pe (in-list (reverse pes))])
             (match pe
               [(CarPE:) (acc-obj/sc #'car obj)]
               [(CdrPE:) (acc-obj/sc #'cdr obj)]
               [(VecLenPE:) (acc-obj/sc #'vector-length obj)]))]
          [(LExp: const terms)
           (linear-exp/sc const
                          (for/hash ([(obj coeff) (in-terms terms)])
                            (values (obj->sc obj) coeff)))]))
(DEBUG "BG type ~a~n" type)
      (cached-match
       sc-cache type typed-side
       ;; Applications of implicit recursive type aliases
       ;;
       ;; We special case this rather than just resorting to standard
       ;; App resolution (see case below) because the resolution process
       ;; will make type->static-contract infinite loop.
       [(App: (Name: name _ #f) _)
(DEBUG "BG App~n")
        ;; Key with (cons name 'app) instead of just name because the
        ;; application of the Name is not necessarily the same as the
        ;; Name type alone
        ;;bg what are examples of things that fall in this case???
        (cond [(hash-ref recursive-values (cons name 'app) #f)]
              [else
               (define name* (generate-temporary name))
               (recursive-sc (list name*)
                             (list
                              (t->sc (resolve-once type)
                                     #:recursive-values
                                     (hash-set recursive-values
                                               (cons name 'app)
                                               (recursive-sc-use name*))))
                             (recursive-sc-use name*))])]
       ;; Implicit recursive aliases
       [(Name: name-id args #f)
(DEBUG "BG Name~n")
        ;;bg: TODO examples
        (cond [;; recursive references are looked up in a special table
               ;; that's handled differently by sc instantiation
               (lookup-name-sc type typed-side)]
              [else
               (define rv recursive-values)
               (define resolved-name (resolve-once type))
               (register-name-sc type
                                 (λ () (loop resolved-name 'untyped rv))
                                 (λ () (loop resolved-name 'typed rv))
                                 (λ () (loop resolved-name 'both rv)))
               (lookup-name-sc type typed-side)])]
       ;; Ordinary type applications or struct type names, just resolve
       [(or (App: _ _) (Name/struct:)) (t->sc (resolve-once type))]
       [(Univ:)
        any/sc]
       [(Bottom:)
        (or/sc)]
       [(Listof: elem-ty)
        list?/sc]
       ;; This comes before Base-ctc to use the Value-style logic
       ;; for the singleton base types (e.g. -Null, 1, etc)
       [(Val-able: v)
        (if (and (c:flat-contract? v)
                 ;; numbers used as contracts compare with =, but TR
                 ;; requires an equal? check
                 (not (number? v))
                 ;; regexps don't match themselves when used as contracts
                 (not (regexp? v)))
            (flat/sc #`(quote #,v))
            (flat/sc #`(flat-named-contract '#,v (lambda (x) (equal? x '#,v))) v))]
       [(Base-name/contract: sym ctc)
        (flat/sc #`(flat-named-contract '#,sym (flat-contract-predicate #,ctc)) sym)]
       [(Distinction: _ _ t) ; from define-new-subtype
        (t->sc t)]
       [(Refinement: par p?)
        (and/sc (t->sc par) (flat/sc p?))]
       [(BaseUnion: bbits nbits)
        (define numeric (make-BaseUnion #b0 nbits))
        (define other-scs (map t->sc (bbits->base-types bbits)))
        (define numeric-sc (numeric-type->static-contract numeric))
        (if numeric-sc
            (apply or/sc numeric-sc other-scs)
            (apply or/sc (append other-scs (map t->sc (nbits->base-types nbits)))))]
       [(? Union? t)
        (match (normalize-type t)
          [(Union-all: elems)
           (apply or/sc (map t->sc elems))]
          [t
           (t->sc t)])]
       [(Intersection: ts raw-prop)
        (define prop*
          (if (TrueProp? raw-prop)
            '()
            (let* ([x (gen-pretty-id)]
                   [prop (Intersection-prop (-id-path x) type)]
                   [name (format "~a" `(λ (,(syntax->datum x)) ,prop))])
              (flat-named-lambda/sc name (id/sc x) (prop->sc prop)))))
        (apply and/sc (append prop* (map t->sc ts)))]
       [(and f (? Fun?))
        (t->sc/fun f)]
       [(Set: t)
        set?/sc]
       [(Sequence: ts)
        sequence?/sc]
       [(Vector: t)
        vector?/sc]
       [(HeterogeneousVector: ts)
        (vectorof/sc (length ts))]
       [(Box: t)
        box?/sc]
       [(Pair: t1 t2)
        cons?/sc]
       [(Async-Channel: t)
        async-channel?/sc]
       [(Promise: t)
        promise?/sc]
       [(Opaque: p?)
        (flat/sc #`(flat-named-contract (quote #,(syntax-e p?)) #,p?))]
       [(Continuation-Mark-Keyof: t)
        continuation-mark-key?/sc]
       [(Prompt-Tagof: _ _)
        prompt-tag?/sc]
       [(F: v)
        ;;bg; an `F` is a free type variable, see `rep/type-rep.rkt` (def-type F)
        any/sc]
       [(VectorTop:)
        vector?/sc]
       [(BoxTop:)
        box?/sc]
       [(ChannelTop:)
        channel?/sc]
       [(Async-ChannelTop:)
        async-channel?/sc]
       [(MPairTop:)
        mpair?/sc]
       [(ThreadCellTop:)
        thread-cell?/sc]
       [(Prompt-TagTop:)
        prompt-tag?/sc]
       [(Continuation-Mark-KeyTop:)
        continuation-mark-key?/sc]
       [(ClassTop:)
        class?/sc]
       [(UnitTop:)
        unit?/sc]
       [(StructTypeTop:)
        struct-type?/sc]
       [(? Poly?)
        (t->sc/poly type fail typed-side recursive-values t->sc)]
       [(? PolyDots?)
        (t->sc/polydots type fail typed-side recursive-values t->sc)]
       [(? PolyRow?)
        (t->sc/polyrow type fail typed-side recursive-values t->sc)]
       [(Mu: n b)
        (match-define (and n*s (list untyped-n* typed-n* both-n*)) (generate-temporaries (list n n n)))
        (define rv
          (hash-set recursive-values n
                    (triple (recursive-sc-use untyped-n*)
                            (recursive-sc-use typed-n*)
                            (recursive-sc-use both-n*))))
        (case typed-side
          [(both) (recursive-sc
                   (list both-n*)
                   (list (loop b 'both rv))
                   (recursive-sc-use both-n*))]
          [(typed untyped)
           (define (rec b side rv)
             (loop b side rv))
           ;; TODO not fail in cases that don't get used
           (define untyped (rec b 'untyped rv))
           (define typed (rec b 'typed rv))
           (define both (rec b 'both rv))
           (recursive-sc
            n*s
            (list untyped typed both)
            (recursive-sc-use (if (from-typed? typed-side) typed-n* untyped-n*)))])]
       ;; Don't directly use the class static contract generated for Name,
       ;; because that will get an #:opaque class contract. This will do the
       ;; wrong thing for object types since it errors too eagerly.
       [(Instance: (? Name? t))
        #:when (Class? (resolve-once t))
        (cond [(lookup-name-sc type typed-side)]
              [else
               (define rv recursive-values)
               (define resolved (make-Instance (resolve-once t)))
               (register-name-sc type
                                 (λ () (loop resolved 'untyped rv))
                                 (λ () (loop resolved 'typed rv))
                                 (λ () (loop resolved 'both rv)))
               (lookup-name-sc type typed-side)])]
       [(Instance: (Class: _ _ fields methods _ _))
        (match-define (list (list field-names field-types) ...) fields)
        (match-define (list (list public-names public-types) ...) methods)
        (object/sc (from-typed? typed-side)
                   (append (map (λ (n sc) (member-spec 'method n sc))
                                public-names (map t->sc/meth public-types))
                           (map (λ (n sc) (member-spec 'field n sc))
                                field-names (map t->sc/both field-types))))]
       [(Class: row-var inits fields publics augments _)
        (match-define (list (list init-names init-types _) ...) inits)
        (match-define (list (list field-names field-types) ...) fields)
        (match-define (list (list public-names public-types) ...) publics)
        (match-define (list (list augment-names augment-types) ...) augments)
        (define-values (pubment-names pubment-types)
          (for/lists (_1 _2) ([name (in-list public-names)]
                              [type (in-list public-types)]
                              #:when (memq name augment-names))
            (values name type)))
        (define-values (override-names override-types)
          (for/lists (_1 _2) ([name (in-list public-names)]
                              [type (in-list public-types)]
                              #:unless (memq name pubment-names))
            (values name type)))
        ;; we need to generate absent clauses for non-opaque class contracts
        ;; that occur inside of a mixin type
        (define absents
          (cond [;; row constraints are only mapped when it's a row polymorphic
                 ;; function in *positive* position (with no sealing)
                 (and (F? row-var) (lookup-row-constraints (F-n row-var)))
                 =>
                 (λ (constraints)
                   ;; the constraints with no corresponding type/contract need
                   ;; to be absent
                   (append (remove* field-names (cadr constraints))
                           (remove* public-names (caddr constraints))))]
                [else null]))
        ;; add a seal/unseal if there was a row variable and the
        ;; row polymorphic function type was in negative position
        (define seal/sc
          (and (F? row-var)
               (not (lookup-row-constraints (F-n row-var)))
               (triple-lookup
                (hash-ref recursive-values (F-n row-var)
                          (λ () (error 'type->static-contract
                                       "Recursive value lookup failed. ~a ~a"
                                       recursive-values (F-n row-var))))
                typed-side)))
        (define sc-for-class
          (class/sc ;; only enforce opaqueness if there's no row variable
           ;; and we are importing from untyped
           (and (from-untyped? typed-side) (not row-var))
           (append
            (map (λ (n sc) (member-spec 'override n sc))
                 override-names (map t->sc/meth override-types))
            (map (λ (n sc) (member-spec 'pubment n sc))
                 pubment-names (map t->sc/meth pubment-types))
            (map (λ (n sc) (member-spec 'inner n sc))
                 augment-names (map t->sc/meth augment-types))
            (map (λ (n sc) (member-spec 'init n sc))
                 init-names (map t->sc/neg init-types))
            (map (λ (n sc) (member-spec 'field n sc))
                 field-names (map t->sc/both field-types)))
           absents))
        (if seal/sc
            (and/sc seal/sc sc-for-class)
            sc-for-class)]
       [(Unit: imports exports init-depends results)
        (define (traverse sigs)
          (for/list ([sig (in-list sigs)])
            (define mapping 
              (map
               (match-lambda 
                 [(cons id type) (cons id (t->sc type))])
               (Signature-mapping sig)))
            (signature-spec (Signature-name sig) (map car mapping) (map cdr mapping))))
        (define imports-specs (traverse imports))
        (define exports-specs (traverse exports))
        (define init-depends-ids (map Signature-name init-depends))
        (match results
          [(? AnyValues?)
           (fail #:reason (~a "cannot generate contract for unit type"
                              " with unknown return values"))]
          [(Values: (list (Result: rngs _ _) ...))
           (unit/sc imports-specs exports-specs init-depends-ids (map t->sc rngs))])]
       [(Struct: nm par (list (fld: flds acc-ids mut?) ...) proc poly? pred?)
        (t->sc (make-Opaque pred?))]
       [(StructType: s)
        struct-type?/sc]
       [(Syntax: t)
        syntax?/sc]
       [(Param: in out)
        parameter?/sc]
       [(Mutable-HashTable: k v)
        mutable-hash?/sc]
       [(Mutable-HashTableTop:)
        mutable-hash?/sc]
       [(Immutable-HashTable: k v)
        immutable-hash?/sc]
       [(Weak-HashTable: k v)
        weak-hash?/sc]
       [(Weak-HashTableTop:)
        weak-hash?/sc]
       [(Channel: t)
        channel?/sc]
       [(Evt: t)
        evt?/sc]
       [(List: elems-pat #:tail tail-pat)
        ;; WARNING: this needs to be the last pattern,
        ;;  because the List: expander doesn't check the head constructor,
        ;;  just "un-tuples" and looks for a sequence ending with tail-pat
        ;;  (tail-pat intended for a dotted list)
        ;; ... really need better way to identify the polydots lists
        ;; TODO make it a stateful check, to make sure all same length?
(DEBUG "POLYDOTS~n")
        list?/sc]
       [rep
        (cond
          [(Prop? rep)
           (fail #:reason "contract generation not supported for this proposition")]
          [else
           (fail #:reason "contract generation not supported for this type")])]))))

;; Generate a contract for a object/class method clause
;; Precondition: type is a valid method type
(define (t->sc/method type fail typed-side recursive-values loop)
  ;; helper for mutually recursive calls in Poly cases
  (define (rec body #:recursive-values rv)
    (t->sc/method body fail typed-side rv loop))
  (match type
    [(? Poly?)
     (t->sc/poly type fail typed-side recursive-values rec)]
    [(? PolyDots?)
     (t->sc/polydots type fail typed-side recursive-values rec)]
    [(? PolyRow?)
     (t->sc/polyrow type fail typed-side recursive-values rec)]
    [(? Fun?)
     (t->sc/function type fail typed-side recursive-values loop #t)]
    [_ (fail #:reason "invalid method type")]))

;; Generate a contract for a polymorphic function type
(define (t->sc/poly type fail typed-side recursive-values t->sc)
  (match-define (Poly: vs b) type)
  ;;bg; no checking for variables
  (let ((recursive-values (for/fold ([rv recursive-values]) ([v vs])
                            (hash-set rv v (same any/sc)))))
    (t->sc b #:recursive-values recursive-values)))

;; Generate a contract for a variable-arity polymorphic function type
(define (t->sc/polydots type fail typed-side recursive-values t->sc)
  (match-define (PolyDots: (list vs ... dotted-v) b) type)
  ;;bg; no checking for variables
  (let ((recursive-values (for/fold ([rv recursive-values]) ([v vs])
                            (hash-set rv v (same any/sc)))))
    (t->sc b #:recursive-values recursive-values)))

;; Generate a contract for a row-polymorphic function type
(define (t->sc/polyrow type fail typed-side recursive-values t->sc)
  (match-define (PolyRow: vs constraints body) type)
  (let ((recursive-values (for/fold ([rv recursive-values]) ([v vs])
                            (hash-set rv v (same any/sc)))))
    (extend-row-constraints vs (list constraints)
      (t->sc body #:recursive-values recursive-values))))

;; Predicate that checks for an App type with a recursive
;; Name type in application position
(define (has-name-app? type)
  (let/ec escape
    (let loop ([rep type])
      (match rep
        [(App: (Name: _ _ #f) _) (escape #t)]
        [_ (Rep-for-each rep loop)]))
    #f))

(module predicates racket/base
  (require racket/extflonum (only-in racket/contract/base >=/c <=/c))
  (provide nonnegative? nonpositive?
           extflonum? extflzero? extflnonnegative? extflnonpositive?)
  (define nonnegative? (>=/c 0))
  (define nonpositive? (<=/c 0))
  (define extflzero? (lambda (x) (extfl= x 0.0t0)))
  (define extflnonnegative? (lambda (x) (extfl>= x 0.0t0)))
  (define extflnonpositive? (lambda (x) (extfl<= x 0.0t0))))

(module numeric-contracts racket/base
  (require
    "../utils/utils.rkt"
    (static-contracts combinators)
    (for-template
      racket/base
      racket/contract
      (submod ".." predicates)
      (prefix-in t: (types numeric-predicates))))
  (provide (all-defined-out))

  (define-syntax-rule (numeric/sc name body)
    (flat/sc #'(flat-named-contract 'name body) 'name))

  (define positive-byte/sc (numeric/sc Positive-Byte (and/c byte? positive?)))
  (define byte/sc (numeric/sc Byte byte?))
  (define positive-index/sc (numeric/sc Positive-Index (and/c t:index? positive?)))
  (define index/sc (numeric/sc Index t:index?))
  (define positive-fixnum/sc (numeric/sc Positive-Fixnum (and/c fixnum? positive?)))
  (define nonnegative-fixnum/sc (numeric/sc Nonnegative-Fixnum (and/c fixnum? nonnegative?)))
  (define nonpositive-fixnum/sc (numeric/sc Nonpositive-Fixnum (and/c fixnum? nonpositive?)))
  (define fixnum/sc (numeric/sc Fixnum fixnum?))
  (define positive-integer/sc (numeric/sc Positive-Integer (and/c exact-integer? positive?)))
  (define natural/sc (numeric/sc Natural exact-nonnegative-integer?))
  (define negative-integer/sc (numeric/sc Negative-Integer (and/c exact-integer? negative?)))
  (define nonpositive-integer/sc (numeric/sc Nonpositive-Integer (and/c exact-integer? nonpositive?)))
  (define integer/sc (numeric/sc Integer exact-integer?))
  (define positive-rational/sc (numeric/sc Positive-Rational (and/c t:exact-rational? positive?)))
  (define nonnegative-rational/sc (numeric/sc Nonnegative-Rational (and/c t:exact-rational? nonnegative?)))
  (define negative-rational/sc (numeric/sc Negative-Rational (and/c t:exact-rational? negative?)))
  (define nonpositive-rational/sc (numeric/sc Nonpositive-Rational (and/c t:exact-rational? nonpositive?)))
  (define rational/sc (numeric/sc Rational t:exact-rational?))
  (define flonum-zero/sc (numeric/sc Float-Zero (and/c flonum? zero?)))
  (define nonnegative-flonum/sc (numeric/sc Nonnegative-Float (and/c flonum? nonnegative?)))
  (define nonpositive-flonum/sc (numeric/sc Nonpositive-Float (and/c flonum? nonpositive?)))
  (define flonum/sc (numeric/sc Float flonum?))
  (define single-flonum-zero/sc (numeric/sc Single-Flonum-Zero (and/c single-flonum? zero?)))
  (define inexact-real-zero/sc (numeric/sc Inexact-Real-Zero (and/c inexact-real? zero?)))
  (define positive-inexact-real/sc (numeric/sc Positive-Inexact-Real (and/c inexact-real? positive?)))
  (define nonnegative-single-flonum/sc (numeric/sc Nonnegative-Single-Flonum (and/c single-flonum? nonnegative?)))
  (define nonnegative-inexact-real/sc (numeric/sc Nonnegative-Inexact-Real (and/c inexact-real? nonpositive?)))
  (define negative-inexact-real/sc (numeric/sc Negative-Inexact-Real (and/c inexact-real? negative?)))
  (define nonpositive-single-flonum/sc (numeric/sc Nonpositive-Single-Flonum (and/c single-flonum? nonnegative?)))
  (define nonpositive-inexact-real/sc (numeric/sc Nonpositive-Inexact-Real (and/c inexact-real? nonpositive?)))
  (define single-flonum/sc (numeric/sc Single-Flonum single-flonum?))
  (define inexact-real/sc (numeric/sc Inexact-Real inexact-real?))
  (define real-zero/sc (numeric/sc Real-Zero (and/c real? zero?)))
  (define positive-real/sc (numeric/sc Positive-Real (and/c real? positive?)))
  (define nonnegative-real/sc (numeric/sc Nonnegative-Real nonnegative?)) ; implies `real?`
  (define negative-real/sc (numeric/sc Negative-Real (and/c real? negative?)))
  (define nonpositive-real/sc (numeric/sc Nonpositive-Real nonpositive?)) ; implies `real?`
  (define real/sc (numeric/sc Real real?))
  (define exact-number/sc (numeric/sc Exact-Number (and/c number? exact?)))
  (define inexact-complex/sc
    (numeric/sc Inexact-Complex
                 (and/c number?
                   (lambda (x)
                     (and (inexact-real? (imag-part x))
                          (inexact-real? (real-part x)))))))
  (define number/sc (numeric/sc Number number?))

  (define extflonum-zero/sc (numeric/sc ExtFlonum-Zero (and/c extflonum? extflzero?)))
  (define nonnegative-extflonum/sc (numeric/sc Nonnegative-ExtFlonum (and/c extflonum? extflnonnegative?)))
  (define nonpositive-extflonum/sc (numeric/sc Nonpositive-ExtFlonum (and/c extflonum? extflnonpositive?)))
  (define extflonum/sc (numeric/sc ExtFlonum extflonum?))

)
(require 'numeric-contracts)

(define (numeric-type->static-contract type)
  (match type
    ;; numeric special cases
    ;; since often-used types like Integer are big unions, this would
    ;; generate large contracts.
    [(== t:-PosByte) positive-byte/sc]
    [(== t:-Byte) byte/sc]
    [(== t:-PosIndex) positive-index/sc]
    [(== t:-Index) index/sc]
    [(== t:-PosFixnum) positive-fixnum/sc]
    [(== t:-NonNegFixnum) nonnegative-fixnum/sc]
    ;; -NegFixnum is a base type
    [(== t:-NonPosFixnum) nonpositive-fixnum/sc]
    [(== t:-Fixnum) fixnum/sc]
    [(== t:-PosInt) positive-integer/sc]
    [(== t:-Nat) natural/sc]
    [(== t:-NegInt) negative-integer/sc]
    [(== t:-NonPosInt) nonpositive-integer/sc]
    [(== t:-Int) integer/sc]
    [(== t:-PosRat) positive-rational/sc]
    [(== t:-NonNegRat) nonnegative-rational/sc]
    [(== t:-NegRat) negative-rational/sc]
    [(== t:-NonPosRat) nonpositive-rational/sc]
    [(== t:-Rat) rational/sc]
    [(== t:-FlonumZero) flonum-zero/sc]
    [(== t:-NonNegFlonum) nonnegative-flonum/sc]
    [(== t:-NonPosFlonum) nonpositive-flonum/sc]
    [(== t:-Flonum) flonum/sc]
    [(== t:-SingleFlonumZero) single-flonum-zero/sc]
    [(== t:-InexactRealZero) inexact-real-zero/sc]
    [(== t:-PosInexactReal) positive-inexact-real/sc]
    [(== t:-NonNegSingleFlonum) nonnegative-single-flonum/sc]
    [(== t:-NonNegInexactReal) nonnegative-inexact-real/sc]
    [(== t:-NegInexactReal) negative-inexact-real/sc]
    [(== t:-NonPosSingleFlonum) nonpositive-single-flonum/sc]
    [(== t:-NonPosInexactReal) nonpositive-inexact-real/sc]
    [(== t:-SingleFlonum) single-flonum/sc]
    [(== t:-InexactReal) inexact-real/sc]
    [(== t:-RealZero) real-zero/sc]
    [(== t:-PosReal) positive-real/sc]
    [(== t:-NonNegReal) nonnegative-real/sc]
    [(== t:-NegReal) negative-real/sc]
    [(== t:-NonPosReal) nonpositive-real/sc]
    [(== t:-Real) real/sc]
    [(== t:-ExactNumber) exact-number/sc]
    [(== t:-InexactComplex) inexact-complex/sc]
    [(== t:-Number) number/sc]
    [(== t:-ExtFlonumZero) extflonum-zero/sc]
    [(== t:-NonNegExtFlonum) nonnegative-extflonum/sc]
    [(== t:-NonPosExtFlonum) nonpositive-extflonum/sc]
    [(== t:-ExtFlonum) extflonum/sc]
    [else #f]))

;; -----------------------------------------------------------------------------
;; bg utils, don't keep these probably

(define (is-a-function-type? initial)
  (let loop ([ty initial])
    (match (resolve ty)
      [(? Fun?) #t]
      [(Union: _ elems) (andmap loop elems)]
      [(Intersection: elems _) (ormap loop elems)]
      [(Poly: _ body) (loop body)]
      [(PolyDots: _ body) (loop body)]
      [_ #f])))

(define (type->function-type type)
  (let loop ([type type])
    (match (resolve type)
     [(and f (? Fun?))
      f]
     [(Poly: _ f)
      (loop f)]
     [_
      (raise-arguments-error 'generate-contract-def/provide
        "failed to convert type for exported function"
        "type" type)])))

;;bg; modeled after type->contract
;; 1. convert domains to static contracts
;; 2. wrap them into a large sc for the function
;; 3. optimize, instantiate
;;
;; Steps (1,2) are extra, type->contract won't do those.
(define (type->function-chaperone-contract type init-fail #:cache cache #:sc-cache sc-cache)
  (let/ec escape
    (define (fail #:reason [reason #f])
      (escape (init-fail #:reason reason)))
    (define sc
      (t->sc/chap-function type fail #:sc-cache sc-cache))
    (instantiate
      (optimize sc #:trusted-positive #true #:trusted-negative #false)
      fail 'impersonator #:cache cache)))

;; Return ArityCtcMap
;;   where
;;   - (define-type ArityCtcMap (Listof (Pairof (U #f ArityKey) CtcMap)))
;;   - (define-type ArityKey (Pairof Natural (Setof Keyword)))
;;   - (define-type CtcMap (HashTable DomainKey (Syntaxof Contract)))
;;   - (define-type DomainKey (U Natural 'rest Keyword))
;; Idea is,
;;  for case-> the arity tells which domain to use
;;  and for everything else, there's a #f entry to use
(define (function-type->domain-contract-map f fail)
  (define typed-side 'typed)
  (define (t->c t)
    ;; TODO add cache
    (type->contract t fail #:kind 'flat #:typed-side #f))
  (match f
   [(Fun: (list arr))
    (define ctcs (make-domain-ctc-map arr t->c))
    (list (cons #f ctcs))]
   [(Fun: arrs)
    (define ctc-map
      (for/fold ([ctc-map '()])
                ([arr (in-list arrs)])
        (define k (make-arity-key arr))
        (define map (make-domain-ctc-map arr t->c))
        (cons (cons k map) ctc-map)))
    ctc-map]
   [_
    (raise-arguments-error 'type->function-macro "cannot convert type to macro" "type" f)]))

(define (make-arity-key arr)
  (match arr
   [(Arrow: dom _ kws _)
    (cons (length dom)
          (sort (map (match-lambda [(Keyword: k _ _) k]) kws) keyword<?))]
   [_ (raise-argument-error 'make-arity-key "Arrow?" arr)]))

(define (make-domain-ctc-map arr t->c)
  (match arr
   [(Arrow: doms rst kws _)
    (define dom-hash
      (for/fold ([H (make-immutable-hash)])
                ([d (in-list doms)]
                 [i (in-naturals)])
        (match-define (list def* ctc) (t->c d))
        (define stx #`(let () #,@def* #,ctc))
        (hash-set H i stx)))
    (define rst-hash
      (if rst
        (match (t->c rst)
         [(list def* ctc)
          (define stx #`(let () #,@def* #,ctc))
          (hash-set dom-hash 'rest ctc)])
        dom-hash))
    (define kwd-hash
      (for/fold ([H rst-hash])
                ([k (in-list kws)])
        (match-define (Keyword: kw kty _) k)
        (match-define (list def* ctc) (t->c kty))
        (define stx #`(let () #,@def* #,ctc))
        (hash-set H kw stx)))
    kwd-hash]
   [_ (raise-argument-error 'make-domain-ctc-map "Arrow?" arr)]))

(define (t->sc/chap-function f fail #:sc-cache sc-cache)
  (define typed-side 'typed)
  (define (t->sc/neg t #:recursive-values [rv (hash)])
    (type->static-contract t fail #:cache sc-cache #:init-recursive-values rv))
  ;; handle-range : Arr (-> Static-Contact) FAIL -> Static-Contract
  ;; Match the range of an arr and determine if a contract can be generated
  ;; and call the given thunk or raise an error
  (define (handle-range arrow convert-arrow)
    (match arrow
      ;; functions with no props or objects
      [(Arrow: _ _ _
               (Values: (list (Result: _
                                       (PropSet: (TrueProp:)
                                                 (TrueProp:))
                                       (Empty:)) ...)))
       (convert-arrow)]
      ;; Functions that don't return
      [(Arrow: _ _ _
               (Values: (list (Result: (== -Bottom) _ _) ...)))
       (convert-arrow)]
      ;; functions with props or objects
      [(Arrow: _ _ _ (Values: (list (Result: rngs _ _) ...)))
       (if (from-untyped? typed-side)
           (fail #:reason (~a "cannot generate contract for function type"
                              " with props or objects."))
           (convert-arrow))]
      [(Arrow: _ _ _ (? ValuesDots?))
       (fail #:reason (~a "cannot generate contract for function type"
                          " with dotted return values"))]
      [(Arrow: _ _ _ (? AnyValues?))
       (fail #:reason (~a "cannot generate contract for function type"
                          " with unknown return values"))]))
  (match f
    [(Fun: arrows)
     ;; Try to generate a single `->*' contract if possible.
     ;; This allows contracts to be generated for functions with both optional and keyword args.
     ;; (and don't otherwise require full `case->')
     (define conv (match-lambda [(Keyword: kw kty _) (list kw (t->sc/neg kty))]))
     (define (partition-kws kws) (partition (match-lambda [(Keyword: _ _ mand?) mand?]) kws))
     (cond
      ;; To generate a single `->*', everything must be the same for all arrs, except for positional
      ;; arguments which can increase by at most one each time.
      ;; Note: optional arguments can only increase by 1 each time, to avoid problems with
      ;;  functions that take, e.g., either 2 or 6 arguments. These functions shouldn't match,
      ;;  since this code would generate contracts that accept any number of arguments between
      ;;  2 and 6, which is wrong.
      ;; TODO sufficient condition, but may not be necessary
      [(has-optional-args? arrows)
       (define first-arrow (first arrows))
       (define last-arrow (last arrows))
       (define (convert-arrow)
         (match-define (Arrow: first-dom rst kws
                               (Values: (list (Result: rngs _ _) ...)))
           first-arrow)
         ;; all but dom is the same for all arrs
         (define last-dom (Arrow-dom last-arrow))
         (define mand-args (map t->sc/neg first-dom))
         (define opt-args (map t->sc/neg (drop last-dom (length first-dom))))
         (define-values (mand-kws opt-kws)
           (let*-values ([(mand-kws opt-kws) (partition-kws kws)])
             (values (map conv mand-kws)
                     (map conv opt-kws))))
         (define range (list any/sc)) ;;bg pre-optimizing the range here
         (define rest (and rst (listof/sc (t->sc/neg rst))))
         (function/sc (from-typed? typed-side) mand-args opt-args mand-kws opt-kws rest range))
       (handle-range first-arrow convert-arrow)]
      [else
       (define ((f case->) a)
         (define (convert-arr arr)
           (match arr
             [(Arrow: dom rst kws (Values: (list (Result: rngs _ _) ...)))
              (let-values ([(mand-kws opt-kws) (partition-kws kws)])
                ;; Garr, I hate case->!
                (when (and (not (empty? kws)) case->)
                  (fail #:reason (~a "cannot generate contract for case function type"
                                     " with optional keyword arguments")))
                (if case->
                  (arr/sc (map t->sc/neg dom)
                          (and rst (listof/sc (t->sc/neg rst)))
                          (list any/sc))
                  (function/sc
                    (from-typed? typed-side)
                    (map t->sc/neg dom)
                    null
                    (map conv mand-kws)
                    (map conv opt-kws)
                    (match rst
                      [(? Type?) (listof/sc (t->sc/neg rst))]
                      [(RestDots: dty dbound)
                       (listof/sc
                        (t->sc/neg dty
                                   #:recursive-values ;;bg TODO ever use this?
                                   (hash-set (hash) dbound (same any/sc))))]
                      [_ #f])
                    (list any/sc))))]))
         (handle-range a (λ () (convert-arr a))))
       (define arities
         (for/list ([t (in-list arrows)]) (length (Arrow-dom t))))
       (define maybe-dup (check-duplicates arities))
       (when maybe-dup
         (fail #:reason (~a "function type has two cases of arity " maybe-dup)))
       (if (= (length arrows) 1)
           ((f #f) (first arrows))
           (case->/sc (map (f #t) arrows)))])]))

