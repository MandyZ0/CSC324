#lang racket
(require "stack_choice.rkt")

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


; Generating expressions
(define plus-rule (lambda (e1 e2) (list '+ e1 e2)))
(define times-rule (lambda (e1 e2) (list '* e1 e2)))
; We use thunks to delay the evaluation of the choice expressions.
(define atom (thunk (-< 1 2 3 4)))
(define rule (thunk (-< plus-rule times-rule)))

(define/match (expression-of-rank k)
  ; A rank 0 expression is a (choice of) atom.
  [(0) (atom)]

  ; A rank 1 expression is a (choice of) rule applied to
  ; (choices of) two rank 0 expressions.
  [(1) ((rule) (expression-of-rank (- k 1)) (expression-of-rank (- k 1)))]

  ; A rank k >= 2 expression is a (choice of) rule applied to one of:
  ;   - (choices of) two rank (k-1) expressions
  ;   - a (choice of) rank (k-1) expression and
  ;     a (choice of) rank (choice of) 0-(k-2)) expression
  ;   - a (choice of) rank (choice of) 0-(k-2)) expression and
  ;     a (choice of) rank (k-1) expression
  [(_)
   (-< ((rule) (expression-of-rank (- k 1)) (expression-of-rank (- k 1)))
       ((rule) (expression-of-rank (- k 1))
               (expression-of-rank (num-between 0 (- k 2))))
       ((rule) (expression-of-rank (num-between 0 (- k 2)))
               (expression-of-rank (- k 1))))])


; Query operator ?- (see backtracking.rkt).
(define (?- pred expr)
  (if (pred expr)
      expr
      (next)))


#|
(find-equal n k)
  n: The target number.
  k: The desired the rank.

  Returns an expression of rank k whose value (when evaluated in Racket) is equal to n.
|#
(define (find-equal n rank)
  (?- (lambda (e) (equal? (eval e) n))
      (expression-of-rank rank)))
