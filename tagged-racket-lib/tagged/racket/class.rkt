#lang racket/base

(require racket/require
         (subtract-in racket/class
                      tagged-racket/base-env/class-prims)
         tagged-racket/base-env/class-prims)

(provide (all-from-out tagged-racket/base-env/class-prims)
         (all-from-out racket/class))
