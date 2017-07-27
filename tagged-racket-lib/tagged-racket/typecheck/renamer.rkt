#lang racket/base

(require tagged-racket/utils/tc-utils)

(provide make-tagged-renaming un-rename)

;; a constructor for typed renamings that attach the required
;; 'not-free-identifier properties
(define (make-tagged-renaming target alternate)
  (tagged-renaming (syntax-property target 'not-free-identifier=? #t)
                  (syntax-property alternate 'not-free-identifier=? #t)))

;; target : identifier
;; alternate : identifier
(struct tagged-renaming (target alternate)
  ;; prevent the rename transformer from expanding in
  ;; module-begin context because the tagged context flag
  ;; will not be set until the module-begin
  #:property prop:expansion-contexts
  '(expression top-level module definition-context)
  ;; delay the rename transformer target selection until
  ;; expansion time when the typed context flag is set correctly
  #:property prop:rename-transformer
  (λ (obj)
    (if (unbox tagged-context?)
        (tagged-renaming-target obj)
        (tagged-renaming-alternate obj))))

;; Undo renaming for type lookup.
;; Used because of macros that mark the identifier used as the binding such as
;; kw-application or struct constructors
;;
;; The syntax-transforming check is for unit tests
(define (un-rename id)
  (if (syntax-transforming?)
      (let-values (((binding new-id) (syntax-local-value/immediate id (lambda () (values #f #f)))))
        (if (tagged-renaming? binding)
            new-id
            id))
      id))
