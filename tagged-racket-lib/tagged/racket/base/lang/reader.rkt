#lang s-exp syntax/module-reader

tagged/racket/base

#:read r:read
#:read-syntax r:read-syntax
#:info make-info
#:language-info make-language-info

(define (make-info key default use-default)
  (case key
    [(drracket:opt-in-toolbar-buttons)
     '(optimization-coach)]
    [else (use-default key default)]))

(define make-language-info
  `#(tagged-racket/language-info get-info ()))


(require (prefix-in r: tagged-racket/tagged-reader))
