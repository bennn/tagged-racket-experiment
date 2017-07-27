#lang racket/base
(require tagged-racket/tagged-reader)
(provide get-info configure)

(define ((get-info arg) key default)
  (case key
    [(configure-runtime) `(#(tagged-racket/language-info configure ()))]
    [else default]))

;; options currently always empty
(define (configure options)
  (namespace-require 'racket/base)
  (eval '(begin
           (require (for-syntax tagged-racket/utils/tc-utils racket/base))
           (begin-for-syntax (set-box! tagged-context? #t)))
        (current-namespace))
  (current-readtable (readtable)))
