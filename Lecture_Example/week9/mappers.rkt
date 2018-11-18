#lang racket

(define num-list (list 1 2 3))
(define num-vec (vector 1 2 3))
(define num-stream (stream 1 2 3))
(define (add10 n) (+ n 10))

(define (add10ToAll nums)
  (cond
    [(list? nums) (map add10 nums)]
    [(vector? nums) (vector-map add10 nums)]
    [(stream? nums) (stream-map add10 nums)]))
