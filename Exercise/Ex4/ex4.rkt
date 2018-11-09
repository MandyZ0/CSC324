#lang racket #| CSC324 Fall 2018: Exercise 4 |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html ★
|#
;-------------------------------------------------------------------------------
(provide analyze-strictness)


;-------------------------------------------------------------------------------
; ★ Task 1: Strictness analysis ★
;-------------------------------------------------------------------------------
#|
(analyze-strictness func-defs)
  func-defs:  '((define (f x y z) (+ x y)))
      A datum representing a program as specified by the grammar on the handout.
      (In this case, it's a list of function definitions.)

  Returns a *strictness map*, which is a hash table mapping function names (as symbols)
  to a list of indexes of the strict parameters of that function.
  Each list should be in increasing order.

  Remember, you may assume that the program has no syntactic or semantic errors
  (so you don't even need to watch out for things like duplicate names).

  Hash table reference: https://docs.racket-lang.org/reference/hashtables.html.

  Implementation hints:
    1. Same as Exercise 3, the main work can be done by processing the list of
       function definitions in a call to foldl.
    2. Working with list indexes is a bit more annoying in pure functional land.
       Use the list function `indexes-where`, which is similar to `filter` except
       it returns indexes rather than elements.
|#
;input: one func def: '(define (f x y) (if x y 5))
;output: a list of indexes on strict position
;smap: existing hash table
(define (analyzehelper smap func-def)
  (let* ([func-name (first(first(rest func-def)))]
         [func-para (rest(first(rest func-def)))]
         [expr (first(rest(rest func-def)))]
         )
    (outputlist smap func-name func-para func-para expr)
    ))
;helper:
;input: func-name func-para 
;output: list
(define (outputlist smap func-name func-para originparalist expr)

  (cond
    [(null? func-para) '()]
    [else
     (let ([first-para (first func-para)]
           [rest-lst (outputlist smap func-name (rest func-para) originparalist expr)])
       (cond
         [(strict-in? smap first-para expr) (append (list (index-where originparalist (same? first-para))) rest-lst)]
         [else rest-lst]))]))

(define (analyze-strictness func-defs)
  (define rfuncdef (reverse func-defs))
  (cond
    [(null? rfuncdef) (define hash (make-immutable-hash))
                      hash]
    [else
     (let* ([first-func-def (first rfuncdef)]
            [first-func-def-name (first(first(rest first-func-def)))]
            [rest-hash (analyze-strictness (rest rfuncdef))])
       (hash-set rest-hash first-func-def-name (analyzehelper rest-hash first-func-def))    
       )]
    ))

#|
(strict-in? s-map id expr) '(define (f x) (if x 3 5))
  s-map: A strictness map
  id: an identifier
  expr: A quoted expression in the grammar specified by the assignment handout.

output: boolean, check whether the identifier is strict in the expression

  Returns whether id is strict in the given expression.
  Uses the given strictness map to determine strictness in function calls.

  NOTE: this function isn't being tested explicitly, so you may freely change it or
  ignore it for this exercise.

  Implementation hint:
    Remember that `and` and `or` aren't identifiers, you can't pass them to
    HOFs like `map` or `apply`. But you can use functions like `andmap` and `ormap`
    to achieve similar effects.
；x is strict in an if-expression i.e. (if <cond> <then> <else>)
 iff x is strict in <cond> or x is strict in both <then> and <else>
|#

#|
;****how can I check whether it's an identifier? 
(define (ident? expr)
  
  (cond
    
  [(number? expr) #f]
  [(list? expr) #f]
  [else  #t])
  )
|#

;*** helper function for plus, function call:
;intput: an datum arguments without + : '(x 1 2 3)
;output: number of arguments strict
(define (plus smap id lst)
  (cond
    [(null? lst) 0]
    [else
     (let* ([first-para (first lst)]
            [rest-numofplus (plus smap id (rest lst))])
       [cond
         [(strict-in? smap id first-para) (+ 1 rest-numofplus)]
         [else rest-numofplus]
         ])]))


(define (strict-in? smap id expr)
  (cond
    ; number
    [(number? expr) #f]
    ;identifier ; it's strict iff x == expr
    [(symbol? expr) 
     
     (cond
       [(equal? id expr) #t]   
       [else #f])]

    ;if expression
    [(equal? 'if (first expr)) 
     (let* ([condexpr (first(rest expr))]
            [thenexpr (first(rest(rest expr)))]
            [elseexpr (first (list-tail expr 3))])
       (cond
         [(strict-in? smap id condexpr) #t]
         [(strict-in? smap id thenexpr) 
                                        (cond
                                          [(strict-in? smap id elseexpr) #t]
                                          [else
           
                                           #f])]
         [else #f]))]
    ;+ expression
    [(equal? '+ (first expr))
     ; need helper function
     (let ([num-of-strict (plus smap id (rest expr))])
       (cond
         [(equal? num-of-strict 0) #f]
         [else #t]))]
    ;function call:
    [else
     (let ([func-name (first expr)]
           [num-of-strict (plus smap id (rest expr))])
       (cond
         [(equal? num-of-strict 0) #f]
         [else
          (cond
            [(equal? (index-of (hash-ref smap func-name) (index-where (rest expr) (lambda (ex) (strict-in? smap id ex)))) #f) #f]
            [else #t])]))]))

;helper for check whether index in the list
;input: list refered by smap
;output: boolean
;(define (checkindex lst index)
; )

(define fmap
  (make-immutable-hash
   (list (cons 'f (list 0))))
  )
;finding strict parameter
(define (same? x)
  (lambda (para)
    (equal? x para)))

#|
we have function declaration, x1: strict parameter
whenever f is called, the value of the argument passed to x1 is
required to evaluate the body of f

input: datum: a list of function definitions.
output: hash table:
            key: function name
            value: a list of indexes 
|#

(module+ test
  (require rackunit)

  (test-equal? "Identity function"
               (analyze-strictness '((define (f x) x)))
               (hash
                'f (list 0)))

  (test-equal? "One function, with if (1)"
               (analyze-strictness '((define (f x) (if x 3 5))))
               (hash
                'f (list 0)))

  (test-equal? "One function, with +"
               (analyze-strictness '((define (f x y z) (+ x y))))
               (hash
                'f (list 0 1)))
  )
