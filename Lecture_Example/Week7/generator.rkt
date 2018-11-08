#lang racket #| Choices and backtracking, Generator Version |#
; This is the same code as stack_choice.rkt, except with a technical addition
; (a "syntax parameter") that enables `-<` and `next` to target a "local-choice"
; identifier rather than the global "choices".
; You are responsible for understanding the concept of generators and the sample
; usage found at the bottom of this file, but NOT how this code was implemented.

#| Usage note

  In the DrRacket menu, go to Language -> Choose Language...
  Click on Show Details, and make sure "Enforce constant definitions"
  is *NOT* checked.
|#
; We import `prompt` from this library, for our implementation of `get-choices`.
(require racket/control)

(provide -<
         next
         choices
         get-choices
         define-generator
         
         ; We didn't talk about these in lecture, but are sometimes useful exports.
         done?
         reset-choices!)

; A constant representing the state of having *no more choices*. 
(define DONE 'done)

; The stack of choice points, represented as a list.
(define choices null)

(require racket/stxparam)
(define-syntax-parameter choices-stack
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! id v) #'(set-choice! v)]
       [id (identifier? #'id)  #'choices]))))
(define (set-choice! v) (set! choices v))

#|
(-< expr ...)
 expr ... : Any expression.

 Evaluates and returns the first expression, but creates a "choice point"
 that can be returned to later with a different expression used.
|#
(define-syntax -<
  (syntax-rules ()
    ; Given no options, return done.
    [(-<) DONE]
  
    ; Given one option, return the value.
    [(-< <expr1>) <expr1>]
    
    ; If there are two or more values, return the first one and
    ; store the others in a thunk,
    ; but *also stores the continuation of the -< expression*.
    [(-< <expr1> <expr2> ...)
     (let/cc cont
       ; 1. Store the current continuation and the remaining choices in a thunk.
       (push! choices-stack
              (thunk (cont (-< <expr2> ...))))

       ; 2. Evaluate and return the first expression.
       <expr1>)]))


#|
(next)

  Returns the next choice, or `done` if there are no more choices.

  For the generator version, we require that next be a macro rather than a function,
  so that it can be used properly inside the syntax-parameterize block below.
|#
(define-syntax-rule (next)
  (if (null? choices-stack)
      ; abort is imported from racket/control. It removes the current
      ; continuation (just like calling another continuation), except
      ; all it does is return its argument; in this case, DONE.
      (abort DONE)
      ; Note that (get-choice!) returns a thunk, which we need to call.
      ((pop! choices-stack))))


;-------------------------------------------------------------------------------
; Stack helpers
;-------------------------------------------------------------------------------
(define-syntax push!
  (syntax-rules ()
    [(push! <id> obj)
     (set! <id>
           (cons obj <id>))]))

(define-syntax pop!
  (syntax-rules ()
    [(pop! <id>)
     (let ([obj (first <id>)])
       (set! <id> (rest <id>))
       obj)]))


;-------------------------------------------------------------------------------
; Other utilities
;-------------------------------------------------------------------------------
#|
(get-choices n)
  n: a non-negative integer

  Returns a list of the next n choices (stored in choice).
  If there are fewer than n choices remaining, returns all of them.
|#
(define (get-choices n)
  (if (equal? n 0)
      empty
      (let* ([v (prompt (next))])
        (if (equal? v DONE)
            empty
            (cons v (get-choices (- n 1)))))))


(define (reset-choices!) (set! choices empty))
(define (done? v) (equal? v DONE))


;-------------------------------------------------------------------------------
; NEW: defining generators
;-------------------------------------------------------------------------------
#|
(define-generator (<id> <params> ...) <body> ...)

  Define a generator, which is a function that returns an object that yields
  values from the given body one at a time by sending it the message 'next.

  One technical limitation of this implementation is that all choice-related
  functions must be defined inside the body of the generator (see the sampleusage below, in which we define num-between right in the generator body)
|#
(define-syntax define-generator
  (syntax-rules ()
    [(define-generator (<generator-name> <params> ...) <body> ...)
     (define (<generator-name> <params> ...)
       ; A local choice stack (inaccesible from outside this macro.
       (define local-choices null)
       (syntax-parameterize ([choices-stack
                              (make-set!-transformer
                               (lambda (stx)
                                 (syntax-case stx (set!)
                                   [(set! id v) #'(set! local-choices v)]
                                   [id (identifier? #'id)  #'local-choices])))])
         ; Save the entire choice expression. prompt is used so that no extra
         ; continuation outside <body> ... is saved when the first choice is evaluated.
         (push! local-choices (thunk (prompt <body> ...)))
         
         (lambda (msg)
           (if (equal? msg 'next)
               (prompt (next))
               (error "Generators only respond to 'next... for now!")))))]))


; Sample usage
(define-generator (my-range start end)
  (define (num-between start end)
    (if (equal? start end)
        (-< start)
        (-< start (num-between (+ 1 start) end))))
  (num-between start end))
