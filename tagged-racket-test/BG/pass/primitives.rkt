#lang tagged/racket

;; Primitives that should not introduce dynamic typechecks

(rest '(1 2 3))
(apply + '(1 2 3))

(struct foo ())
(foo? 4)
