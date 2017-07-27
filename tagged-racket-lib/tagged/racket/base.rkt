#lang tagged-racket/minimal

(providing (libs (except racket/base #%module-begin #%top-interaction
                         with-handlers default-continuation-prompt-tag
                         define λ lambda define-struct for for*
                         let let* let-values let*-values letrec letrec-values
                         let/cc let/ec do case-lambda struct
                         for/list for/vector for/hash for/hasheq for/hasheqv
                         for/and for/or for/sum for/product for/lists
                         for/first for/last for/fold for*/list for*/lists
                         for*/vector for*/hash for*/hasheq for*/hasheqv for*/and
                         for*/or for*/sum for*/product for*/first for*/last
                         for*/fold))
           (basics #%module-begin #%top-interaction))

(require tagged-racket/base-env/extra-procs
         (except-in tagged-racket/base-env/prims
           require-typed-struct-legacy
           require/typed-legacy
           require-typed-signature)
         tagged-racket/base-env/base-types
         (except-in tagged-racket/base-env/base-types-extra Distinction Unit))
(provide (rename-out [define-type-alias define-type])
         (all-from-out tagged-racket/base-env/prims)
         (all-from-out tagged-racket/base-env/base-types)
         (all-from-out tagged-racket/base-env/base-types-extra)
         assert defined? with-type for for*)
