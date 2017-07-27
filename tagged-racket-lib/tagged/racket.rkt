#lang tagged-racket/minimal

(require tagged/racket/base racket/require
         (subtract-in racket tagged/racket/base racket/contract
                      tagged/racket/class
                      tagged/racket/unit)
         tagged/racket/class
         tagged/racket/unit
	 (for-syntax racket/base))
(provide (all-from-out racket
                       tagged/racket/base
                       tagged/racket/class
                       tagged/racket/unit)
	 (for-syntax (all-from-out racket/base))
         class)
