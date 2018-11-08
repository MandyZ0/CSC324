#lang racket #| Backtracking library |#
(require "stack_choice.rkt")


#|
(prime-sum start end)
  start end: numbers  (Precondition: start <= end)

  Return a list (a b (a+b)) where a and b are numbers in the range [start..end],
  and (a+b) is prime.
|#
(define (prime-sum start end)
  (let* ([a (num-between start end)]
         [b (num-between start end)]
         [c (+ a b)])
    (?- (lambda (lst) (prime? (third lst))) (list a b c))))


#|
(?- pred expr)
  pred: A predicate
  expr: A choice expression

  Returns a choice from expr that satisfies the given predicate.
  Returns 'done is no choice satisfies the predicate.
|#
(define (?- pred expr)
  (if (pred expr)
      expr
      (next)))


;-------------------------------------------------------------------------------
; Helpers
;-------------------------------------------------------------------------------
#|
(prime? n)
  n: A positive integer

  Returns whether n is prime. (Brute-force trial division algorithm.)
|#
(define (prime? n)
  (not (ormap (lambda (x) (equal? (remainder n x) 0)) (range 2 n))))


#|
(num-between start end)
  start: an integer
  end: an integer

  Returns a choice of a number between start and end.
  Precondition: start <= end.
|#
(define (num-between start end)
  (if (equal? start end)
      (-< start)
      (-< start
          (num-between (+ start 1) end))))
