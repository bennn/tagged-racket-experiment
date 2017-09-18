#lang tagged/racket

(struct foo ([x : Symbol]))

(: g (-> foo Boolean))
(define (g f)
  (match-define (foo q) f)
  (symbol=? q 'rrr))

