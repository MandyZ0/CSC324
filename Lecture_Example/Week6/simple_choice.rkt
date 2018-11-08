#lang racket #| Choices and backtracking, Version 1 |#
; This file illustrates the basic implementation of the ambiguous operator -<.
; Note that while each (-< ...) saves the remaining choices in next-choice,
; it does *not* save the execution context of the expression.


#| Usage note

  In the DrRacket menu, go to Language -> Choose Language...
  Click on Show Details, and make sure "Enforce constant definitions"
  is *NOT* checked.
|#

; A constant representing the state of having *no more choices*.
(define DONE 'done)

#|
A (private) global variable storing the result of choice expressions.
This should either be (void) or a *thunk* that both returns a value and mutates
`next-choice`.

You can think about this value as a self-mutating stream (loosely, when the "rest thunk"
is called, rather than returning a lazy list, it stores it in `next-choice`).
|#
(define next-choice (void))


(define-syntax -<
  (syntax-rules ()
    ; Given no options, reset `next-choice` and return 'done.
    [(-<)
     (begin
       (set-next-choice! (void))
       DONE)]

    ; If there are one or more values, return the first one and
    ; store the others in a thunk.
    [(-< <expr1> <expr2> ...)
     (begin
       ; 1. Update `next-choice` to store a *thunk* that wraps the
       ;    remaining expressions. None of them are evaluated.
       (set-next-choice! (thunk (-< <expr2> ...)))

       ; 2. Evaluate and return the first expression.
       <expr1>)]))

#|
Mutating helper. Uses set! to update next-choice.
|#
(define (set-next-choice! v)
  (set! next-choice v))


#|
(next)

  Returns the next choice,
  or 'done if there are no more choices.
|#
(define (next)
  (if (void? next-choice)
      DONE
      (next-choice)))


; Sample usage
(module+ main
  (-< 1 2 3)     ; Evaluates to 1.
  (next)         ; Evaluates to 2.
  (next)         ; Evaluates to 3.
  (next)         ; Evaluates to 'done.
  )
