#lang racket #| Choices and backtracking, Version 2 |#
; This version builds on version 1 in simple_choice.rkt by storing both the
; choices (-< ...) AND current continuation in next-choice.

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

    ; Given one option, reset `next-choice` and return the value.
    ; This prevents us from calling `(cont (-<))` in the recursive
    ; case below.
    [(-< <expr1>)
     (begin
       (set-next-choice! (void))
       <expr1>)]

    ; If there are two or more values, return the first one and
    ; store the others in a thunk in `next-choice`,
    ; but *also stores the continuation of the -< expression*.
    [(-< <expr1> <expr2> ...)

     (let/cc cont
       ; 1. Store the current continuation and the remaining choices in a thunk.
       (set-next-choice! (thunk (cont (-< <expr2> ...))))

       ; 2. Evaluate and return the first expression.
       <expr1>)]))


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

    ; Illustrating the use of continuations!
    ; The continuation stored is (+ 10 _).
    (+ 10 (-< 1 2 3))  ; Evaluates to 11.
    (next)             ; Evaluates to 12.
    (next)             ; Evaluates to 13.
    (next)             ; Evaluates to 'done.
    )


#|
Mutating helper. Uses set! to update next-choice.
|#
(define (set-next-choice! v)
  (set! next-choice v))


#|
(get-choices n)
  n: a non-negative integer

  Returns a list of the next n choices (stored in next-choice).
  If there are fewer than n choices remaining, returns all of them.

  NOTE: this implementation doesn't work! We'll explore how to
  fix it next class.
|#
(define (get-choices n)
  (if (equal? n 0)
      empty
      (let* ([v (next)])
        (if (equal? v DONE)
            empty
            (cons v (get-choices (- n 1)))))))


;-------------------------------------------------------------------------------
; save-cc macro demo
;-------------------------------------------------------------------------------
(define saved-cont (void))

(define-syntax save-cc
  (syntax-rules ()
    [(save-cc <expr>)
     (let/cc cont
       (set-saved-cont! cont)
       <expr>)]))

; Need a helper for a technical reason to use save-cc in the REPL.
(define (set-saved-cont! v)
  (set! saved-cont v))
