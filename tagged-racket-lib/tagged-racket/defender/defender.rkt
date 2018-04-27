#lang racket/base

(require
  (only-in racket/format ~a)
  racket/match
  syntax/id-set
  syntax/parse
  tagged-racket/rep/type-rep
  tagged-racket/rep/values-rep
  tagged-racket/static-contracts/logger
  tagged-racket/types/match-expanders
  (only-in tagged-racket/typecheck/internal-forms
    typed-struct
    typed-struct/exec)
  (only-in tagged-racket/types/base-abbrev
    make-Listof)
  (only-in tagged-racket/types/abbrev
    -Void
    -String
    -Symbol
    -True
    -False
    ->
    -values)
  tagged-racket/types/struct-table
  tagged-racket/types/type-table
  tagged-racket/types/union
  tagged-racket/types/utils
  (only-in tagged-racket/private/syntax-properties
    type-ascription-property
    type-inst-property
    ignore^
    ignore-some^
    opt-lambda^
    kw-lambda^)
  (only-in racket/syntax
    format-id
    generate-temporary)
  (only-in (submod tagged-racket/private/type-contract test-exports)
    type->contract)
  (for-syntax
    racket/base)
  (for-template
    racket
    racket/unsafe/ops
    tagged-racket/types/numeric-predicates
    (submod racket/performance-hint begin-encourage-inline)))

(provide defend-top)

(module+ test
  (require rackunit))

;; =============================================================================

(define (defend-top stx ctc-cache sc-cache extra-defs*)
  (let loop ([stx stx])
    (syntax-parse stx
     #:literals (values define-values #%plain-app begin define-syntaxes)
     [_
      #:when (is-ignored? stx) ;; lookup in type-table's "ignored table"
      stx]
     [(~or _:ignore^ _:ignore-some^) ;; for struct definitions ... not sure what else
      stx]
     [((~literal define-syntaxes) . _)
      stx]
     [(~and _:kw-lambda^ ((~literal let-values) ([(f) fun]) body))
      (syntax/loc stx (let-values ([(f) fun]) body))]
     [(~and _:opt-lambda^ ((~literal let-values) ([(f) fun]) body))
      (syntax/loc stx (let-values ([(f) fun]) body))]
     [(op:lambda-identifier formals . body)
      (define dom-map (type->domain-map (stx->arrow-type stx) #f))
      (with-syntax ([body+ (loop #'body)]
                    [formals+ (protect-formals dom-map #'formals ctc-cache sc-cache extra-defs*)])
        (syntax/loc stx
          (op formals (void . formals+) . body+)))]
     #;[(define-values (var ...) expr) (TODO need this?)]
     [(x* ...)
      #:when (is-application? stx)
      (define stx+
        (datum->syntax stx
          (map loop (syntax-e #'(x* ...)))))
      (define-values [pre* f post*] (split-application stx+))
      (if (is-ignored? f)
        stx+
        (let ()
          ;; TODO don't use orig?
          (define-values [_orig-pre* orig-f orig-post*] (split-application stx))
          (define-values [dom-types cod-type]
            (let ([ty (stx->arrow-type orig-f (length (syntax-e orig-post*)))])
              (values (type->domain-map ty f) (type->codomain-type ty f))))
          (define stx/dom
            (with-syntax ([(pre ...) pre*]
                          [f f]
                          [(dom ...) (protect-domain dom-types post* ctc-cache sc-cache extra-defs*)])
              (syntax/loc stx+ (pre ... f dom ...))))
          (define stx/cod
            (protect-codomain cod-type stx/dom ctc-cache sc-cache extra-defs*))
          stx/cod))]
     [((~and x (~literal #%expression)) _)
      #:when (type-inst-property #'x)
      stx]
     [((~literal #%expression) e)
      #:when (type-ascription-property stx)
      (with-syntax ([e+ (loop #'e)])
        (syntax/loc stx (#%expression e+)))]
     [_
      #:when (type-ascription-property stx)
      (raise-user-error 'defend-top "strange type-ascription ~a" (syntax->datum stx))]
     [(x* ...)
      (datum->syntax stx
        (map loop (syntax-e #'(x* ...))))]
     [_
      stx])))

(define-syntax-class lambda-identifier
  (pattern (~literal #%plain-lambda))
  (pattern (~literal lambda)))

;; -----------------------------------------------------------------------------

;; Higher-order functions that respect their function arguments.
(define BLESSED-DOMAIN (immutable-free-id-set (map (lambda (sym) (format-id #'#f "~a" sym)) '(
  build-vector
))))

(define BLESSED-CODOMAIN (immutable-free-id-set (map (lambda (sym) (format-id #'#f "~a" sym)) '(
  ;; --- 4.1
  boolean? not equal? eqv? eq? equal?/recur immutable? symbol=? boolean=?
  false? nand nor implies xor
  ;; --- 4.2.1
  number? complex? real? rational? integer? exact-integer? exact-nonnegative-integer?
  exact-positive-integer? inexact-real? fixnum? flonum? double-flonum? single-flonum?
  zero? positive? negative? even? odd? exact? inexact? inexact->exact exact->inexact
  real->single-flonum real->double-flonum
  ;; --- 4.2.2
  + - * / quotient remainder quotient/remainder
  modulo add1 sub1 abs max min gcd lcm round floor ceiling truncate numerator
  denominator rationalize = < <= > >= sqrt integer-sqrt integer-sqrt/remainder
  expt exp log sin cos tan asin acos atan make-rectangular make-polar real-part
  imag-part magnitude angle bitwise-ior bitwise-and bitwise-xor bitwise-not
  bitwise-bit-set? bitwise-bit-field arithmetic-shift integer-length random
  random-seed make-pseudo-random-generator pseudo-random-generator? current-pseudo-random-generator
  pseudo-random-generator->vector vector->pseudo-random-generator! pseudo-random-generator-vector?
  crypto-random-bytes random-sample number->string string->number real->decimal-string
  integer-bytes->integer integer->integer-bytes floating-point-bytes->real
  real->floating-point-bytes system-big-endian? degrees->radians radians->degrees
  sqr sgn conjugate sinh cosh tanh exact-round exact-floor exact-ceiling exact-truncate
  order-of-magnitude nan? infinite? positive-integer? negative-integer? nonpositive-integer?
  nonnegative-integer? natural?
  ;; --- 4.2.3
  fl+ fl- fl* fl/ flabs fl= fl< fl> fl<= fl>= flmin flmax flround flfloor
  flceiling fltruncate flsin flcos fltan flasin flacos flatan fllog flexp flsqrt
  flexpt -->fl fl->exact-integer make-flrectangular flreal-part flimag-part
  flrandom flvector? flvector make-flvector flvector-length flvector-ref
  flvector-set! flvector-copy in-flvector shared-flvector make-shared-flvector
  ;; --- 4.2.4
  fx+ fx- fx* fxquotient fxremainder fxmodulo fxabs fxand fxior fxxor fxnot
  fxlshift fxrshift fx- fx< fx> fx<= fx>= fxmin fxmax fx->fl fl->fx fxvector?
  fxvector make-fxvector fxvector-length fxvector-ref fxvector-set! fxvector-copy
  in-fxvector shared-fxvector make-shared-fxvector
  ;; --- 4.2.5
  extflonum? extflonum-available? extfl+ extfl- extfl* extfl/ extflabs extfl=
  extfl< extfl> extfl<= extfl>= extflmin extflmax extflround extflfloor extflceiling
  extfltruncate extflsin extflcos extfltan extflasin extflacos extflatan extfllog
  extflexp extflsqrt extflexpr ->extfl extfl->exact-integer real->extfl extfl->exact
  extfl->inexact extflvector? extflvector make-extflvector extflvector-length
  extflvector-ref extflvector-set! extflvector-copy in-extflvector make-shared-extflvector
  floating-point-bytes->extfl extfl->floating-point-bytes
  ;; --- 4.3
  string? make-string string string->immutable-string string-length string-ref
  string-set! substring string-copy string-copy! string-fill! string-append
  string->list list->string build-string string=? string<? string<=? string>?
  string>=? string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?
  string-upcase string-downcase string-titlecase string-foldcase string-normalize-nfd
  string-normalize-nfkd string-normalize-nfc string-normalize-nfkc string-locale=?
  string-locale<? string-locale>? string-locale-ci=? string-locale-ci<? string-locale-ci>?
  string-locale-upcase string-locale-downcase string-append* string-join string-normalize-spaces
  string-replace string-split string-trim non-empty-string? string-contains?
  string-prefix? string-suffix? ~a ~v ~s ~e ~r ~.a ~.v ~.s
  ;; --- 4.4
  bytes? make-bytes bytes bytes->immutable-bytes byte? bytes-length bytes-ref
  bytes-set! subbytes bytes-copy bytes-copy! bytes-fill! bytes-append
  bytes->list list->bytes make-shared-bytes shared-bytes bytes=? bytes<?
  bytes>? bytes->string/utf-8 bytes->string/locale bytes->string/latin-1
  string->bytes/utf-8 string->bytes/locale string->bytes/latin-1 string-utf-8-length
  bytes-utf-8-length bytes-utf-8-ref bytes-utf-8-index bytes-open-converter
  bytes-close-converter bytes-convert bytes-convert-end bytes-converter?
  locale-string-encoding bytes-append* bytes-join
  ;; --- 4.5
  char? char->integer integer->char char-utf-8-length char=? char<? char<=?
  char>? char>=? char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=? char-alphabetic?
  char-lower-case? char-upper-case? char-title-case? char-numeric? char-symbolic?
  char-punctuation? char-graphic? char-whitespace? char-iso-control? char-general-category
  make-known-char-range-list char-upcase char-downcase char-titlecase char-foldcase
  ;; --- 4.6
  symbol? symbol-interned? symbol-unreadable? symbol->string string->symbol
  string->uninterned-symbol string->unreadable-symbol gensym symbol<?
  ;; --- 4.7
  regexp? pregexp? byte-regexp? byte-pregexp? regexp pregexp byte-regexp
  byte-pregexp regexp-quote regexp-max-lookbehind regexp-match regexp-match*
  regexp-try-match regexp-match-positions regexp-match-positions* regexp-match?
  regexp-match-exact? regexp-match-peek regexp-match-peek-positions
  regexp-match-peek-immediate regexp-match-peek-positions-immediate
  regexp-match-peek-positions* regexp-match/end regexp-match-positions/end
  regexp-match-peek-positions/end regexp-match-peek-positions-immediate/end
  regexp-split regexp-replace regexp-replace* regexp-replaces regexp-replace-quote
  ;; --- 4.8
  keyword? keyword->string string->keyword keyword<?
  ;; --- 4.9
  pair? null? cons list? list list* build-list length list-tail append reverse
  map andmap ormap for-each filter remove remq remv remove* remq* remv* sort
  member memv memq memf findf assoc assv assq assf cons? empty? rest make-list
  list-update list-set index-of index-where indexes-of indexes-where take takef
  drop-right takef-right list-prefix? take-common-prefix add-between append*
  flatten check-duplicates remove-duplicates filter-map count range append-map
  filter-not shuffle combinations in-combinations permutations in-permutations
  group-by cartesian-product remf remf* make-reader-graph placeholder?
  make-placeholder placeholder-set! hash-placeholder? make-hash-placeholder
  make-hasheq-placeholder make-hasheqv-placeholder
  ;; --- 4.10
  mpair? mcons set-mcar! set-mcdr!
  ;; --- 4.11
  vector? make-vector vector vector-immutable vector-length vector-set!
  vector->list list->vector vector->immutable-vector vector-fill! vector-copy!
  build-vector vector-set*! vector-map vector-map! vector-append vector-take
  vector-take-right vector-drop vector-drop-right vector-split-at
  vector-split-at-right vector-copy vector-filter vector-filter-not
  vector-count vector-member vector-memv vector-memq vector-sort vector-sort!
  ;; --- 4.12
  box? box box-immutable set-box! box-cas!
  ;; --- 4.13
  hash? hash-equal? hash-eqv? hash-eq? hash-weak? hash hasheq hasheqv make-hash
  make-hasheqv make-hasheq make-weak-hash make-weak-hasheqv make-weak-hasheq
  make-immutable-hash make-immutable-hasheqv make-immutable-hasheq
  hash-set! hash-set*! hash-set hash-set* hash-has-key? hash-update! hash-update
  hash-remove! hash-remove hash-clear! hash-clear hash-copy-clear hash-map
  hash-keys hash-values hash->list hash-keys-subset? hash-for-each hash-count
  hash-empty? hash-iterate-first hash-iterate-next hash-copy eq-hash-code
  eqv-hash-code equal-hash-code equal-secondary-hash-code hash-union hash-union!
  ;; --- 4.14.1.1
  sequence? in-range in-naturals in-list in-mlist in-vector in-string in-bytes
  in-port in-input-port-bytes in-input-port-chars in-lines in-bytes-lines
  in-hash in-hash-keys in-hash-values in-hash-pairs
  in-mutable-hash in-mutable-hash-keys in-mutable-hash-values in-mutable-hash-pairs
  in-immutable-hash in-immutable-hash-keys in-immutable-hash-values in-immutable-hash-pairs
  in-weak-hash in-weak-hash-keys in-weak-hash-values in-weak-hash-pairs
  in-directory in-producer in-value in-indexed in-sequences in-cycle in-parallel
  in-values-sequence in-values*-sequence stop-before stop-after make-do-sequence
  ;; --- 4.14.1.2
  sequence->stream sequence-generate sequence-generate*
  ;; --- 4.14.1.3
  sequence->list sequence-length sequence-ref sequence-tail sequence-append
  sequence-map sequence-andmap sequence-ormap sequence-for-each sequence-fold
  sequence-count sequence-filter sequence-add-between sequence/c
  ;; --- 4.14.1.3.1
  in-syntax in-slice
  ;; --- 4.14.2
  stream? stream-empty? stream-first stream-rest stream-cons stream stream*
  in-stream stream->list stream-length stream-tail stream-append stream-map
  stream-andmap stream-ormap stream-for-each stream-fold stream-count stream-filter
  stream-add-between stream/c
  ;; --- 4.14.3
  generator? generator yield infinite-generator in-generator generator-state
  sequence->generator sequence->repeated-generator
  ;; --- 4.15
  dict? dict-implements? dict-implements/c dict-mutable? dict-can-remove-keys?
  dict-can-functional-set? dict-set! dict-set dict-remove! dict-remove dict-iterate-first
  dict-iterate-next dict-iterate-key dict-iterate-value dict-has-key? dict-set*!
  dict-set* dict-update! dict-update dict-map dict-for-each dict-empty?
  dict-count dict-copy dict-clear dict-clear! dict-keys dict-values dict->list
  in-dict in-dict-keys in-dict-values in-dict-pairs dict-key-contract dict-value-contract
  dict-iter-contract define-custom-hash-types make-custom-hash-types make-custom-hash
  make-weak-custom-hash make-immutable-custom-hash
  ;; --- 4.16.1
  set-equal? set-eqv? set-eq? set? set-mutable? set-weak? set seteqv seteq mutable-seteqv
  mutable-seteq weak-set weak-seteqv weak-seteq list->set list->seteqv list->seteq
  list->mutable-set list->mutable-seteqv list->mutable-seteq list->weak-set list->weak-seteqv
  list->weak-seteq for/set for/seteq for/seteqv for*/set for*/seteq for*/seteqv
  for/mutable-set for/mutable-seteq for/mutable-seteqv for*/mutable-set for*/mutable-seteq
  for*/mutable-seteqv for/weak-set for/weak-seteq for/weak-seteqv for*/weak-set
  for*/weak-seteq for*/weak-seteqv in-immutable-set in-mutable-set in-weak-set
  generic-set? set-implements? set-implements/c set/c set-member? set-add set-add!
  set-remove set-remove! set-empty? set-count set->stream set-copy set-copy-clear
  set-clear set-clear! set-union set-union! set-intersect set-intersect! set-subtract
  set-subtract! set-symmetric-difference set-symmetric-difference! set=? subset?
  proper-subset? set->list set-map set-for-each in-set impersonate-hash-set impersonate-hash-set
  chaperone-hash-set define-custom-set make-custom-set-types
  ;; --- 4.17
  procedure? apply compose compose1 procedure-rename procedure->method procedure-closure-contents-eq?
  keyword-apply procedure-arity procedure-arity? procedure-arity-includes? procedure-reduce-arity
  procedure-keywords procedure-result-arity make-keyword-procedure procedure-reduce-keyword-arity
  procedure-struct-type? procedure-extract-target checked-procedure-check-and-extract
  procedure-specialize primitive? primitive-closure? primitive-result-arity identity
  const thunk thunk* negate conjoin disjoin curry curryr normalized-arity? normalize-arity
  arity=? arity-includes?
  ;; --- 4.18
  void? void
  ;; --- 4.19
  ;; --- 5.2
  make-struct-type make-struct-field-accessor make-struct-field-mutator make-struct-type-property
  struct-type-property? struct-type-property-accessor-procedure?
  ;; --- 5.5

  ;; --- 10.1
  values
  ;; --- 10.2
  raise error raise-user-error raise-argument-error raise-arguments-error
  raise-result-error raise-range-error raise-type-error raise-mismatch-error
  raise-arity-error raise-syntax-error unquoted-printing-string? unquoted-printing-string
  unquoted-printing-string-value uncaught-exception-handler error-escape-handler
  error-display-handler error-print-width error-print-context-length error-value->string-handler
  error-print-source-location exn->string

  ;; --- 13.5
  write display print writeln displayln println fprintf printf eprintf format
  print-pair-curly-braces print-mpair-curly-braces print-unreadable print-graph
  print-struct print-box print-vector-length print-hash-table print-boolean-long-form
  print-reader-abbreviations print-as-expression print-syntax-width current-write-relative-directory
  port-write-handler port-display-handler port-print-handler global-port-print-handler

  ;; --- 14.1
  namespace? make-empty-namespace make-base-empty-namespace namespace-anchor?
  namespace-anchor->empty-namespace namespace-anchor->namespace current-namespace
  namespace-symbol->identifier namespace-base-phase namespace-module-identifier
  namespace-module-identifier namespace-set-variable-value! namespace-undefine-variable!
  namespace-mapped-symbols namespace-require namespace-require/copy namespace-require/constant
  namespace-require/expansion-time namespace-attach-module namespace-attach-module-declaration
  namespace-unprotect-module namespace-module-registry module->namespace
  namespace-syntax-introduce module-provide-protected? variable-reference?
  variable-reference-constant? variable-reference->empty-namespace variable-reference->namespace
  variable-reference->resolved-module-path variable-reference->module-path-index
  variable-reference->module-source variable-reference->phase variable-reference->module-base-phase
  variable-reference->module-declaration-inspector


  ;; --- 15.6
  current-seconds current-inexact-milliseconds seconds->date date-second
  date-minute date-hour date-day date-month date-year date-week-day date-year-day
  date-dst? date-time-zone-offset date*-nanosecond date*-time-zone-name
  current-milliseconds current-process-milliseconds current-gc-milliseconds
  time-apply time
  ;; --- 15.6.1
  current-date date->string date-display-format date->seconds date*->seconds
  find-seconds date->julian/scalinger julian/scalinger->string

  ;; --- 17
  unsafe-fx+ unsafe-fx- unsafe-fx* unsafe-fxquotient unsafe-fxremainder unsafe-fxmodulo
  unsafe-fxabs unsafe-fxand unsafe-fxior unsafe-fxxor unsafe-fxnot unsafe-fxlshift
  unsafe-fxrshift unsafe-fx= unsafe-fx< unsafe-fx> unsafe-fx<= unsafe-fx>=
  unsafe-fxmin unsafe-fxmax unsafe-fl+ unsafe-fl- unsafe-fl* unsafe-fl/ unsafe-flabs
  unsafe-fl= unsafe-fl< unsafe-fl> unsafe-fl<= unsafe-fl>= unsafe-flmin unsafe-flmax
  unsafe-flround unsafe-flfloor unsafe-flceiling unsafe-fltruncate unsafe-flsin
  unsafe-flcos unsafe-fltan unsafe-flasin unsafe-flacos unsafe-flatan unsafe-fllog
  unsafe-flexp unsafe-flsqrt unsafe-flexpt unsafe-make-flrectangular unsafe-flreal-part
  unsafe-flimag-part unsafe-fx->fl unsafe-fl->fx unsafe-flrandom unsafe-car
  unsafe-cdr unsafe-mcar unsafe-mcdr unsafe-set-mcar! unsafe-set-mcdr! unsafe-cons-list
  unsafe-list-ref unsafe-list-tail unsafe-unbox unsafe-set-box! unsafe-unbox*
  unsafe-set-box*! unsafe-box*-cas! unsafe-vector-length unsafe-vector-set!
  unsafe-vector*-length unsafe-vector*-set! unsafe-string-length unsafe-string-set!
  unsafe-bytes-length unsafe-bytes-set! unsafe-fxvector-length unsafe-fxvector-set!
  unsafe-flvector-length unsafe-flvector-set! unsafe-f64vector-set! unsafe-s16vector-set!
  unsafe-u16-vector-set! unsafe-struct-set! unsafe-struct*-set! unsafe-extfl+
  unsafe-extfl- unsafe-extfl* unsafe-extfl/ unsafe-extflabs unsafe-extfl= unsafe-extfl<
  unsafe-extfl> unsafe-extfl<= unsafe-extfl>= unsafe-extflmin unsafe-extflmax
  unsafe-extflround unsafe-extflfloor unsafe-extflceiling unsafe-extfltruncate
  unsafe-extflsin unsafe-extflcos unsafe-extfltan unsafe-extflasin unsafe-extflacos
  unsafe-extflatan unsafe-extfllog unsafe-extflexp unsafe-extflsqrt unsafe-extflexpt
  unsafe-fx->extfl unsafe-extfl->fx unsafe-extflvector-length unsafe-extflvector-set!

  ;; --- Typed Racket
  index? exact-rational?
))))

;; -----------------------------------------------------------------------------

;; is-application? : Syntax -> Boolean
;; Returns #true if `stx` is a function application (an app that may need dynamic checking)
(define (is-application? stx)
  (syntax-parse stx
   [((~literal #%plain-app) . _)
    (has-type-annotation? stx)]
   [_
    #false]))

;; split-application : Syntax -> (Values (Syntaxof List) Syntax (Syntaxof List))
(define (split-application stx)
  (syntax-parse stx
   #:literals (#%plain-app)
   #:datum-literals (apply)
   [((~and a #%plain-app) (~and b apply) f . arg*)
    (values #'(a b) #'f #'arg*)]
   [((~and a #%plain-app) f . arg*)
    (values #'(a) #'f #'arg*)]
   [_
    (raise-argument-error 'split-application "(Syntaxof App)" stx)]))

(define (stx->arrow-type stx [num-args #f])
  (define raw-type (tc-results->type (maybe-type-of stx)))
  (let loop ([ty (and raw-type (normalize-type raw-type))])
    (match ty
     [(Fun: (list (? Arrow?)))
      ty]
     [(Param: in out)
      (cond
       [(not num-args)
        (raise-arguments-error 'stx->arrow-type "cannot coerce parameter to arrow type, number of arguments is unknown" "type" ty "stx" stx)]
       [(= 0 num-args)
        (-> out)]
       [(= 1 num-args)
        (-> in -Void)]
       [else
        (raise-arguments-error 'stx->arrow-type "wrong number of arguments supplied to parameter" "type" ty "stx" stx "num-args" num-args)])]
     [(Poly: _ b)
      (loop b)]
     [(Fun: arrs)
      ;;bg; if case->, try combining the arrs to a union type
      ;;    this is possible when each `arr` has the same arity
      (define arr (combine-arrs arrs))
      (if arr
        (loop (make-Fun (list arr)))
        (raise-arguments-error 'dynamic-typecheck "failed to parse arrow from case->"
          "type" ty
          "e" (syntax->datum stx)
          "stx" stx
          "cases" arrs))]
     [_
      (raise-arguments-error 'dynamic-typecheck "failed to parse arrow from type of syntax object"
        "e" (syntax->datum stx)
        "stx" stx
        "type" ty)])))

;; combine-arrs : (-> (Listof Arrow) (U #f Arrow))

(module+ test
  (check-equal?  ;;bg; not happy about ~a ....
    (~a (combine-arrs (list
        (make-Arrow (list -Symbol -True (make-Listof -Symbol) Univ) #f '() (-values (list -String)))
        (make-Arrow (list -False -False (make-Listof -Symbol) Univ) #f '() (-values (list -String)))
        (make-Arrow (list -Symbol Univ (make-Listof -Symbol) -True) #f '() (-values (list -String)))
        (make-Arrow (list -Symbol Univ -False -False) #f '() (-values (list -String))))))
    (~a (make-Arrow (list (Un -Symbol -False)
                          (Un -True -False Univ)
                          (Un (make-Listof -Symbol) -False)
                          (Un Univ -True -False))
                    #f '() (-values (list -String)))))
)

(define (combine-arrs arrs)
  (match arrs
   [(list (Arrow: t** #f '() rng*) ...)
    #:when (same-length? t**)
    (define m+ (combine-dom* t**))
    (define rng+ (combine-rng* rng*))
    (make-Arrow m+ #f '() rng+)]
   [_
    #f]))

(define (same-length? x**)
  (or (null? x**)
      (null? (cdr x**))
      (and (= (length (car x**)) (length (cadr x**)))
           (same-length? (cdr x**)))))

(define (combine-dom* t**)
  (if (andmap null? t**)
    '()
    (cons (apply Un (map car t**)) (combine-dom* (map cdr t**)))))

(define (combine-rng* rng*)
  (define t** (map some-values->type* rng*))
  (and (same-length? t**)
       (let ([t* (combine-dom* t**)])
         (make-Values t*))))

(define (tc-results->type r)
  (match r
   [(tc-result1: t)
    t]
   [_
    #f]))

(define REST-KEY 'rest)

;; type->domain-map : Type Syntax -> TypeMap
;;   where TypeMap = (HashTable (U Fixnum 'rest Keyword) (U #f Type))
;; Build a TypeMap from the domain of an arrow type.
;; Use `stx` to decide whether to remove some types.
(define (type->domain-map t stx)
  (match t
   [(or (Fun: (list (Arrow: mand rst kws _)))
        (Arrow: mand rst kws _))
    (define trusted?
      (blessed-domain? stx))
    (define mand-hash
      ;; Map positional arguments
      (for/fold ([acc (make-immutable-hash)])
                ([d (in-list mand)]
                 [i (in-naturals)])
        (hash-set acc i (if trusted? #f d))))
    (define rst-hash
      (cond
       [(Type? rst)
        (hash-set mand-hash REST-KEY (if trusted? #f (make-Listof rst)))]
       [(RestDots? rst)
        (raise-arguments-error 'type->domain-map "type without rest-dots"
          "type" t
          "stx" stx)]
       [else
        mand-hash]))
    (define kwd-hash
      ;; Map keyword args
      (for/fold ([acc rst-hash])
                ([k (in-list kws)])
        (match k
         [(Keyword: kw ty _)
          (hash-set acc kw (if trusted? #f kw))]
         [_
          (raise-arguments-error 'type->domain-map "arrow type (with good keywords)" "type" t "stx" stx)])))
    kwd-hash]
   [_
    (raise-argument-error 'type->domain-map "arrow type" t)]))

(define (type-map-ref map key)
  (define (fail-thunk)
    (raise-arguments-error 'type-map-ref "unbound key" "key" key "map" map))
  (cond
   [(fixnum? key)
    (hash-ref map key (λ () (hash-ref map REST-KEY fail-thunk)))]
   [(keyword? key)
    (hash-ref map key fail-thunk)]
   [(eq? REST-KEY key)
    (hash-ref map key fail-thunk)]
   [else
    (raise-argument-error 'type-map-ref "(or/c fixnum? 'rest keyword?)" 1 map key)]))

;; type->codomain-type : Type Syntax -> (U #f SomeValues)
;; Get the codomain from an arrow type,
;;  use `stx` to decide whether we can skip the codomain check.
(define (type->codomain-type t stx)
  (match t
   [(Fun: (list (Arrow: _ _ _ cod)))
    (if (blessed-codomain? stx)
      #f
      cod)]
   [_
    (raise-argument-error 'type->cod-type "arrow type" t)]))

(define (blessed-domain? stx)
  (and stx #t) ;; TRUST EVERYTHING, typed functions are fully defensive
  #;(if (identifier? stx)
    (or (free-id-set-member? BLESSED-DOMAIN stx)
        (tagged-racket-identifier? stx))
    #f))

(define (blessed-codomain? stx)
  (if (identifier? stx)
    (or (syntax-property stx 'constructor-for)
        ;; too hard to find `struct-predicate-procedure?`s
        (free-id-set-member? BLESSED-CODOMAIN stx)
        (and (tagged-racket-identifier? stx)
             (not (struct-accessor? stx))
             (not (from-require/typed? stx))))
    (is-lambda? stx)))

;; from-require/typed? : Identifier -> Boolean
;; Typed Racket adds this property to all require/typed identifiers,
;;  see `utils/require-contract.rkt`
(define (from-require/typed? stx)
  (syntax-property stx 'not-provide-all-defined))

(define (tagged-racket-identifier? stx)
  (define ib (identifier-binding stx))
  (and (pair? ib)
       (or (identifier-binding-from-this-module? ib)
           (identifier-binding-from-tagged-racket-module? ib))))

(define (identifier-binding-from-this-module? ib)
  (match ib
   [(list src-mpi _src-id nom-src-mpi nom-src-id 0 0 0)
    (and (equal? src-mpi (module-path-index-join #f #f))
         (equal? src-mpi nom-src-mpi))]
   [_
    #false]))

(define (identifier-binding-from-tagged-racket-module? ib)
  (match ib
   [(list src-mpi _src-id _nom-src-mpi _nom-src-id 0 0 0)
    (tagged-racket-mpi? src-mpi)]
   [_
    #false]))

(define tagged-racket-mpi?
  (let ([cache (make-hash)])
    (λ (mpi)
      (hash-ref! cache mpi
        (λ () ;; Typed Racket always installs a `#%type-decl` submodule
          (let* ([mpi+ (module-path-index-join '(submod "." #%type-decl) mpi)])
            (parameterize ([current-namespace (make-base-namespace)])
              (with-handlers ([exn:fail:contract? (lambda (exn) #f)])
                (and mpi+
                     (dynamic-require mpi+ #f)
                     #t)))))))))

;; protect-domain : TypeMap (Syntaxof List) Hash Hash (Boxof Syntax) -> (Syntaxof List)
;; Return a copy of `dom-stx` that has tag-safety checks.
;;  (these checks might be chaperones!)
(define (protect-domain dom-map dom-stx ctc-cache sc-cache extra-defs*)
  (define dom+
    (let loop ([dom* (syntax-e dom-stx)]
               [position 0])
      (cond
       [(null? dom*)
        '()]
       [(keyword? (syntax-e (car dom*)))
        (define k-stx (car dom*))
        (define v-stx (cadr dom*))
        (define t (type-map-ref dom-map (syntax-e k-stx)))
        (define v+ (protect-arg t v-stx ctc-cache sc-cache extra-defs*))
        (list* k-stx v+ (loop (cddr dom*) position))]
       [else
        (define t (type-map-ref dom-map position))
        (define v+ (protect-arg t (car dom*) ctc-cache sc-cache extra-defs*))
        (cons v+ (loop (cdr dom*) (+ position 1)))])))
  (with-syntax ([dom+ dom+])
    (syntax/loc dom-stx dom+)))

;; protect-arg : (U #f Type) Syntax Hash Hash (Boxof Syntax) -> Syntax
(define (protect-arg dom-type dom-stx ctc-cache sc-cache extra-defs*)
  (cond
   [(not dom-type)
    dom-stx]
   [(not (needs-domain-check? dom-type))
    dom-stx]
   [else
    ;; TODO should put this in a function ... because `contract` is too expensive
    (define (fail #:reason r)
      (raise-user-error 'dynamic-typecheck "failed to convert arrow type ~a to chaperone because ~a" dom-type (syntax-e dom-stx)))
    (match-define (list defs ctc-stx)
      (type->contract dom-type fail
        #:typed-side #t
        #:kind 'impersonator
        #:cache ctc-cache
        #:sc-cache sc-cache
        #:contract-depth 1))
    (set-box! extra-defs* (append (reverse defs) (unbox extra-defs*)))
    (with-syntax ([ctc ctc-stx]
                  [dom dom-stx])
      (syntax/loc dom-stx (contract ctc dom 'tagged-world 'dynamic-typecheck)))]))

;; protect-codomain : (U #f SomeValues) (Syntaxof List) Hash Hash (Boxof Syntax) -> (Syntaxof List)
(define (protect-codomain cod-type app-stx ctc-cache sc-cache extra-defs*)
  (cond
   [(not cod-type)
    app-stx]
   [else
    (define t*
      (let ([t* (some-values->type* cod-type)])
        (if (null? t*)
          (raise-argument-error 'protect-codomain "non-empty SomeValues" cod-type)
          t*)))
    (define ctc-stx* ;; (Listof (U #f Syntax))
      (for/list ([t (in-list t*)])
        (type->flat-contract t ctc-cache sc-cache extra-defs*)))
    (define err-msg
      (parameterize ([error-print-width 20])
        (format "~e : ~a" (syntax->datum app-stx) cod-type)))
    (if (not (ormap values ctc-stx*))
      ;; Nothing to check
      app-stx
      ;; Assemble everything into a syntax object that:
      ;; - performs the application
      ;; - binds the result(s) to temporary variable(s)
      ;; - checks the tag of each temporary
      (with-syntax ([app app-stx]
                    [err err-msg])
        (define var-name 'dyn-cod)
        (if (null? (cdr t*))
          ;; -- application returns 1 result, just bind it and check it
          (with-syntax ([(ctc) ctc-stx*]
                        [v (generate-temporary var-name)])
            (syntax/loc app-stx
              (let ([v app])
                (if ((begin-encourage-inline ctc) v)
                  v
                  (error 'dynamic-typecheck (format "~e : ~a" v 'err))))))
          ;; - application returns +1 results:
          ;;   - bind all,
          ;;   - check the ones with matching contracts,
          ;;   - return all
          (with-syntax ([v* (for/list ([_t (in-list t*)])
                               ;; should be OK to do this instead of `generate-temporaries`, right?
                               (generate-temporary var-name))])
            (quasisyntax/loc app-stx
              (let-values ([v* app])
                (if (and . #,(for/list ([ctc-stx (in-list ctc-stx*)]
                                        [v (in-list (syntax-e #'v*))]
                                        #:when ctc-stx)
                               (quasisyntax/loc app-stx ((begin-encourage-inline #,ctc-stx) #,v))))
                  (values . v*)
                  (error 'dynamic-typecheck 'err))))))))]))

;; protect-formals : TypeMap (Syntaxof List) Hash Hash (Boxof Syntax) -> (Syntaxof List)
(define (protect-formals dom-map formals ctc-cache sc-cache extra-defs*)
  (define xs
    (let loop ([dom* formals] [position 0])
      ;; TODO ... kinda similar to protect-domain
      ;;  wow this is off the hook  ... sometimes called with (a b (c . d))
      (cond
       [(null? dom*)
        '()]
       [(not (pair? dom*))
        (cond
         [(identifier? dom*)
          (define t (type-map-ref dom-map REST-KEY))
          (list (protect-codomain t (datum->syntax formals dom*) ctc-cache sc-cache extra-defs*))]
         [(syntax? dom*)
          (loop (syntax-e dom*) position)]
         [else
          (raise-arguments-error 'protect-formals "strange domain element in formals"
            "elem" dom*
            "formals" formals)])]
       [(keyword? (syntax-e (car dom*)))
        (raise-arguments-error 'protect-formals "unexpected keyword in domain"
          "elem" (car dom*)
          "formals" formals)]
       [else
        (define var (formal->var (car dom*)))
        (define t (type-map-ref dom-map position))
        (cons (protect-codomain t var ctc-cache sc-cache extra-defs*)
              (loop (cdr dom*) (+ position 1)))])))
  (datum->syntax formals xs))

(define (formal->var stx)
  (syntax-parse stx
   [_:id
    stx]
   [(x:id _)
    (syntax/loc stx x)]))

;; some-values->type* : (U Type SomeValues) -> (Listof Type)
(define (some-values->type* sv)
  (match sv
   [(? Type?)
    (list sv)]
   [(Values: r*)
    (map Result-t r*)]
   [(AnyValues: _)
    (raise-user-error 'dynamic-typecheck "cannot generate contract for AnyValues type '~a'" sv)]
   [(ValuesDots: _ _ _)
    (raise-user-error 'dynamic-typecheck "cannot generate contract for ValuesDots type '~a'" sv)]))

(define (is-lambda? x)
  (syntax-parse x
   [((~or (~literal lambda) (~literal #%plain-lambda)) . _) #true]
   [_ #false]))

(define (has-type-annotation? x)
  (match (maybe-type-of x)
   [(tc-result1: _) #true]
   [_ #false]))

(define (needs-domain-check? t)
  ;; TODO recursion is similar to `function-type?` in `type-contract.rkt`
  (match t
   [(Fun: arrs)
    (ormap arr/non-empty-domain? arrs)]
   [(Union: _ elems)
    (ormap needs-domain-check? elems)]
   [(Intersection: elems _)
    (andmap needs-domain-check? elems)]
   [(Poly: _ body)
    (needs-domain-check? body)]
   [(PolyDots: _ body)
    (needs-domain-check? body)]
   [_ #f]))

(define (arr/non-empty-domain? arr)
  (match arr
   [(Arrow: '() #f '() _)
    #false]
   [(Arrow: _ _ _ _)
    #true]
   [_
    (raise-argument-error 'arr/non-empty-domain "Arrow?" arr)]))

(define (type->flat-contract t ctc-cache sc-cache extra-defs*)
  (define (fail #:reason r)
    (raise-user-error 'dynamic-typecheck "failed to convert type ~a to flat contract because ~a" t r))
  (match-define (list defs ctc)
    (type->contract t fail
      #:typed-side #f
      #:kind 'impersonator ;;bg; don't care, depth=0 should minimize it
      #:cache ctc-cache
      #:sc-cache sc-cache
      #:contract-depth 0))
  (set-box! extra-defs* (append (reverse defs) (unbox extra-defs*)))
  (if (free-identifier=? ctc #'any/c) #f ctc))

