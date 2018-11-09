#lang racket
(define (count n)
  (if (equal? n 0)
      0
      (+ 1 (count (- n 1)))))

; What the expansion looks like, roughly:
; (count 100)
; (+ 1 (count 99))
; (+ 1 (+ 1 (count 98)))
; (+ 1 (+ 1 (+ ... (+ 1 (count 0)) ... )))


(define (count-tail n acc)
  (if (equal? n 0)
      acc
      (count-tail (- n 1) (+ acc 1))))

; What the expansion looks like, roughly:
; (count-tail 100 0)
; (count-tail 99 1)
; (count-tail 98 2)
; ...
; (count-tail 0 100)
