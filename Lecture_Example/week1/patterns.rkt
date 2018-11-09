#lang racket
(define/match (f x)
  [(0) 10]
  [(1) 20]
  [(x) (+ x 30)])

(define/match (g lst)
  [((list)) 10]
  [((cons x xs)) (+ x (length xs))])
