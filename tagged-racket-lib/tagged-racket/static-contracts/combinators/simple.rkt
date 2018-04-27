#lang racket/base

;; Static contracts that are terminal and have no sub parts.
;; Unlike contracts defined with define-terminal-contract, equality of these contracts is based solely
;; on identity. Thus they are most useful for contracts which have no meaningful structure.
;; Ex: (flat/sc #'number?)

(require
  "../../utils/utils.rkt"
  "../structures.rkt"
  "../constraints.rkt"
  racket/match
  (contract-req))

(provide/cond-contract
 [flat/sc ((syntax?) ((or/c #f any/c)) . ->* . static-contract?)]
 [chaperone/sc ((syntax?) ((or/c #f any/c)) . ->* . static-contract?)]
 [impersonator/sc ((syntax?) ((or/c #f any/c)) . ->* . static-contract?)])

(define (simple-contract-write-proc v port mode)
  (match-define (simple-contract syntax kind name _) v)
  (define-values (open close)
    (if (equal? mode 0)
        (values "(" ")")
        (values "#<" ">")))
  (display open port)
  (fprintf port "~a/sc" kind)
  (display " " port)
  (write (or name (syntax->datum syntax)) port)
  (display close port))


;; check equality of two syntax objects by structural traversal
;; where identifiers are compared by free-identifier=?
;;
;; Note: does not handle cycles but there shouldn't be any
(define (stx-equal? s1 s2)
  (cond [(and (identifier? s1) (identifier? s2))
         (free-identifier=? s1 s2)]
        [else
         (if (and (syntax? s1) (syntax? s2))
             (equal?/recur (syntax-e s1) (syntax-e s2) stx-equal?)
             (equal?/recur s1 s2 stx-equal?))]))

(struct simple-contract static-contract (syntax kind name maybe-tag-syntax)
        #:transparent
        #:methods gen:equal+hash
         [(define (equal-proc s1 s2 recur)
            (and ;; have to make sure identifiers are compared by free-id=?
                 ;; because of struct predicates, opaque, etc.
                 (stx-equal? (simple-contract-syntax s1)
                             (simple-contract-syntax s2))
                 (recur (simple-contract-kind s1)
                        (simple-contract-kind s2))
                 (recur (simple-contract-name s1)
                        (simple-contract-name s2))))
          (define (hash-proc sc hash-code)
            (bitwise-ior (hash-code (syntax->datum (simple-contract-syntax sc)))
                         (hash-code (simple-contract-kind sc))
                         (hash-code (simple-contract-name sc))))
          (define (hash2-proc sc hash-code)
            (bitwise-ior (hash-code (syntax->datum (simple-contract-syntax sc)))
                         (hash-code (simple-contract-kind sc))
                         (hash-code (simple-contract-name sc))))]
        #:methods gen:sc
         [(define (sc-map v f) v)
          (define (sc-traverse v f) (void))
          (define (sc->contract v f) (simple-contract-syntax v))
          (define (sc->tag-sc v f)
            (case (simple-contract-kind v)
             [(flat)
              (let ([tag-stx (simple-contract-maybe-tag-syntax v)])
                (if tag-stx
                  (simple-contract tag-stx 'flat (simple-contract-name v) #f)
                  v))]
             [(chaperone)
              (let ([tag-stx (simple-contract-maybe-tag-syntax v)])
                (if tag-stx
                  (simple-contract tag-stx 'flat (simple-contract-name v) #f)
                  (raise-user-error 'sc->tag-contract "chaperone simple contract with no tag-stx ~a" v)))]
             [(impersonator)
              (raise-user-error 'sc->tag-contract "impersonator simple contract ~a" v)]))
          (define (sc->constraints v f) (simple-contract-restrict (simple-contract-kind v)))
          (define (sc-terminal-kind v) (simple-contract-kind v))]
        #:methods gen:custom-write [(define write-proc simple-contract-write-proc)])

(define (flat/sc ctc [name #f] #:tag [tag-stx #f])
  (simple-contract ctc 'flat name tag-stx))
(define (chaperone/sc ctc [name #f] #:tag [tag-stx #f])
  (simple-contract ctc 'chaperone name tag-stx))
(define (impersonator/sc ctc [name #f])
  (simple-contract ctc 'impersonator name #f))
