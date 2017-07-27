#lang tagged/racket/base

;; The predicate for `(Mutable-HashTable K V)`
;;  should be mutable-hash?
;;  and NOT list?
;; (Yes this is currently a bug)

(make-hasheq)
