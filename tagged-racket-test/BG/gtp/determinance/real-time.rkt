#lang tagged/racket

(provide enqueue head tail
         queue queue->list empty?)

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

(: queue->list : (All (A) ((Queue A) -> (Listof A))))
(define (queue->list rtq)
  (if (empty? rtq)
      null
      (cons (head rtq) (queue->list (tail rtq)))))

(: list->queue : (All (A) ((Listof A) -> (Queue A))))
(define (list->queue lst)
  (foldl (inst enqueue A) 
         ((inst Queue A) empty-stream null empty-stream) lst))

(: queue : (All (A) (A * -> (Queue A))))
(define (queue . lst)
  (foldl (inst enqueue A) 
         ((inst Queue A) empty-stream null empty-stream) lst))

