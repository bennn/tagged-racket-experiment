#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               "base"
	       "net-lib"
	       "web-server-lib"
               "db-lib"
               "draw-lib"
               "rackunit-lib"
               "rackunit-gui"
               "snip-lib"
               "tagged-racket-lib"
               "gui-lib"
               "pict-lib"
               "images-lib"
               "racket-index"
               "sandbox-lib"))

(define pkg-desc "Tagged Racket Types for various libraries")
;; TODO really should not be necessary ... just re-interpret the TR types

(define pkg-authors '(ben))

(define version "1.9")
