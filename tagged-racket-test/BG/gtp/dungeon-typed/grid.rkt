#lang tagged/racket

(provide
  left
  right
  up
  down
  grid-ref
  grid-height
  grid-width
  show-grid
  array-set!
  build-array
)

(require
  ;"base/cell-types.rkt"
  "cell.rkt")

;; =============================================================================
;; TODO can I use a supertype? Getting errors with init-fields
(define-type CCTable (HashTable Char (U Door% Cell%)))

(define-type Cell%
  (Class
    (init-field
     (items (Listof Any) #:optional)
     (occupant (U #f (Instance Cell%)) #:optional))
    (free? (-> Boolean))
    (open (-> Void))
    (show (-> Char))
    (close (-> Void))))

(define-type Door% Cell%)
;  (Class
;   #:implements/inits Cell%
;   (init-field
;     (open? Boolean #:optional))))

(define-type Pos (Vector Index Index))

(define-type Grid (Vectorof (Vectorof (Instance Cell%))))
;(define-type Grid (Mutable-Array (Instance Cell%)))

;; =============================================================================

(: array-set! (-> Grid Pos (Instance Cell%) Void))
(define (array-set! g p v)
  (vector-set! (vector-ref g (vector-ref p 0)) (vector-ref p 1) v))

(: build-array (-> Pos (-> Pos (Instance Cell%)) Grid))
(define (build-array p f)
  (for/vector : Grid ([x (in-range (vector-ref p 0))])
   (for/vector : (Vectorof (Instance Cell%))
                ([y (in-range (vector-ref p 1))])
    (f (vector (assert x index?) (assert y index?))))))
  ;(build-array p f)))

;; a Grid is a math/array Mutable-Array of cell%
;; (mutability is required for dungeon generation)

;; parses a list of strings into a grid, based on the printed representation
;; of each cell
(: parse-grid (-> (Listof String) Grid))
(define (parse-grid los)
  (for/vector : Grid
              ; #:shape (vector (length los)
              ;                (apply max (map string-length los)))
              ;#:fill (new void-cell%)
              ([s (in-list los)])
            (for/vector : (Vectorof (Instance Cell%))
               ([c (in-string s)]) ;: (Instance Cell%)
     (new (char->cell% c)))))

(: show-grid (-> Grid String))
(define (show-grid g)
  (with-output-to-string
    (lambda ()
      (for ([r (in-vector g)])
        (for ([c (in-vector r)])
          (display (send c show)))
        (newline)))))

(: grid-height (-> Grid Index))
(define (grid-height g)
  (vector-length g))
  ;(match-define (vector rows cols) (array-shape g))
  ;rows)

(: grid-width (-> Grid Index))
(define (grid-width g)
  (vector-length (vector-ref g 0)))
  ;(match-define (vector rows cols) (array-shape g))
  ;cols)

(: within-grid? (-> Grid Pos Boolean))
(define (within-grid? g pos)
  (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
       (<= 0 (vector-ref pos 1) (sub1 (grid-width  g)))))

(: grid-ref (-> Grid Pos (U #f (Instance Cell%))))
(define (grid-ref g pos)
  (and (within-grid? g pos)
       (vector-ref (vector-ref g (vector-ref pos 0)) (vector-ref pos 1))))

(: left (->* (Pos) (Index) Pos))
(define (left pos [n 1])
  (vector (vector-ref pos 0)
          (assert (- (vector-ref pos 1) n) index?)))

(: right (->* (Pos) (Index) Pos))
(define (right pos [n 1])
  (vector (vector-ref pos 0)
          (assert (+ (vector-ref pos 1) n) index?)))

(: up (->* (Pos) (Index) Pos))
(define (up pos [n 1])
  (vector (assert (- (vector-ref pos 0) n) index?)
          (vector-ref pos 1)))

(: down (->* (Pos) (Index) Pos))
(define (down pos [n 1])
  (vector (assert (+ (vector-ref pos 0) n) index?)
          (vector-ref pos 1)))

