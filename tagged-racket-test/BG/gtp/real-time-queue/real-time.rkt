#lang tagged/racket

(provide enqueue head tail
         empty? qmap)

(require "stream.rkt")

(struct: (A) Queue ([front : (Stream A)]
                    [rear  : (Listof A)]
                    [scdul : (Stream A)]))

;; An empty queue
(define-syntax-rule (empty A)
  ((inst Queue A) empty-stream null empty-stream))

;; Function to check for empty queue
(: empty? : (All (A) ((Queue A) -> Boolean)))
(define (empty? rtq)
  (empty-stream? (Queue-front rtq)))


(: rotate : (All (A) ((Stream A) (Listof A) (Stream A) -> (Stream A))))
(define (rotate frnt rer accum)
  (let ([carrer (car rer)])
    (if (empty-stream? frnt)
        (stream-cons carrer accum)
        (stream-cons (stream-car frnt)
                     ((inst rotate A) (stream-cdr frnt) 
                                      (cdr rer) 
                                      (stream-cons carrer accum))))))

(: internal-queue : (All (A) ((Stream A) (Listof A) (Stream A) -> (Queue A))))
(define (internal-queue front rear schdl)
  (if (empty-stream? schdl)
      (let ([newf (rotate front rear schdl)])
        (Queue newf null newf))
      (Queue front rear (stream-cdr schdl))))


(: enqueue : (All (A) (A (Queue A) -> (Queue A))))
(define (enqueue elem rtq)
  (internal-queue (Queue-front rtq)
                  (cons elem (Queue-rear rtq))
                  (Queue-scdul rtq)))


(: head : (All (A) ((Queue A) -> A)))
(define (head rtq)
  (let ([front (Queue-front rtq)])
    (if (empty-stream? front)
        (error 'head "given queue is empty")
        (stream-car front))))


(: tail : (All (A) ((Queue A) -> (Queue A))))
(define (tail rtq)
  (let ([front (Queue-front rtq)])
    (if (empty-stream? front)
        (error 'tail "given queue is empty")
        (internal-queue (stream-cdr front) 
                        (Queue-rear rtq) 
                        (Queue-scdul rtq)))))

;; similar to list map function. apply is expensive so using case-lambda
;; in order to saperate the more common case
(: qmap : 
   (All (A C B ...) 
        (case-lambda 
          ((A -> C) (Queue A) -> (Queue C))
          ((A B ... B -> C) (Queue A) (Queue B) ... B -> (Queue C)))))
(define qmap
  (pcase-lambda: (A C B ...)
                 [([func : (A -> C)]
                   [deq  : (Queue A)])
                  (map-single ((inst Queue C) empty-stream null empty-stream) 
                              func deq)]
                 [([func : (A B ... B -> C)]
                   [deq  : (Queue A)] . [deqs : (Queue B) ... B])
                  (apply map-multiple ((inst Queue C) empty-stream null empty-stream) 
                         func deq deqs)]))


(: map-single : (All (A C) ((Queue C) (A -> C) (Queue A) -> (Queue C))))
(define (map-single accum func que)
  (if (empty? que)
      accum
      (map-single (enqueue (func (head que)) accum) func (tail que))))

(: map-multiple : 
   (All (A C B ...) 
        ((Queue C) (A B ... B -> C) (Queue A) (Queue B) ... B -> (Queue C))))
(define (map-multiple accum func que . ques)
  (if (or (empty? que) (ormap empty? ques))
      accum
      (apply map-multiple
             (enqueue (apply func (head que) (map head ques)) accum)
             func 
             (tail que)
             (map tail ques))))

