#lang racket #| CSC324 Fall 2018: Lab 2 |#
#|
SUMMARY: 
 Higher-order functions, and more practice with lists
	1. num-Pred - a function that takes another function as an argument. (ONE TPYE OF HOF)
		a. return the number of items in  the list which make the function return #t
	2. HOF (Second Type Of HOF: return a function)
		a. make-counter: 
			i. input: FUNCITON: predicate (boolean funciton)
                        ii. output: FUNCTION.  the number of elements in the list satisfying that predicate.
|#

(define (num-evens numbers)

  (cond
    [(null? numbers)
     ; In this case the list is empty. What should be returned?
     0 ]
    [else
     ; In this case the list is non-empty. Divide the list into its first
     ; element and the remaining numbers in the list, recurse on the remaining,
     ; and combine the results in some way to return the final result.
     (let ([first-number (first numbers)]
            [rest-num (num-evens (rest numbers))])  ; Pick a better variable name.
       [cond [(even? first-number) (+ 1 rest-num)]
             [else rest-num]]
 
       )])
  )



(define (num-many-evens lists-of-numbers)
  (cond [(null? lists-of-numbers) 0 ]
        [else
         (let* ([first-num (num-evens (first lists-of-numbers))]
                [rest-num (num-many-evens (rest lists-of-numbers))]
                )
           [cond [(positive? (- first-num 2)) (+ 1 rest-num)]
                 [else rest-num]])]))

;-------------------------------------------------------------------------------
; ★ Task 1: num-pred ★
;-------------------------------------------------------------------------------


(require rackunit)

(define (num-pred pred lst)
  (cond
    [(null? lst) 0 ]
    [else
     (let* ([first-number (first lst)]
            [rest-num (num-pred pred (rest lst))])
      [(pred first-number) (+ 1 rest-num) rest-num]
       )
     ]
    )
  )

(define (LargerThan2 lst)
  (cond [(positive? (- (num-evens lst) 2)) #t]
        [else #f])
  )
(module+ test


  (test-equal? "num-pred/num-evens"
               (num-evens (list 1 2 3 4 5))
               (num-pred even? (list 1 2 3 4 5)))
  


  
  ; TODO: replace the ellipsis with the appropriate predicate for this test to pass.
  (test-equal? "num-pred/num-many-evens"
               (num-many-evens (list (list 1 2 3 4 5) (list 2 4 6 10)))

               (num-pred LargerThan2
                         (list (list 1 2 3 4 5) (list 2 4 6 10)))))


;-------------------------------------------------------------------------------
; ★ Task 2: make-counter ★
;-------------------------------------------------------------------------------
;input: a pred function
;output: a function that take in a list and return the number bla bla
(define (make-counter pred)
  (lambda (f lst)
    (f pred lst))
  )

; Uncomment and define these using `make-counter`.
(define (num-evens-1 lst)
  (num-pred (make-counter even?) lst)
  )
(define (num-many-evens-1 lst)
  (num-pred (make-counter LargerThan2) lst))

  (test-equal? "num-pred/num-evens"
               (num-evens (list 1 2 3 4 5))
               (num-evens-1 (list 1 2 3 4 5)))

;-------------------------------------------------------------------------------
; ★ Task 3: Manipulating Racket expressions ★
;-------------------------------------------------------------------------------

#|
(extract-ints datum)
  datum: A Racket datum (i.e., quoted expression).

  Returns a list of all *integer* literals appearing in the expression.
  The list should contain the literals in the left-to-right order they appear
  in the text of the quoted expression.

  Use the predicates `list?` or `null?` to check for whether a given expression
  is a list or an empty list, respectively. You may assume that if the input
  is not a list, then it is an atom (either a symbol or some sort of literal).

  Hint: you may need a recursive helper function to accumulate the results of
  calling extract-ints recursively. Or, look up the append, map, and append-map
  list functions to see if you get any ideas!

  While you may assume the program is syntactically valid, it is not necessarily
  semantically valid. For example, it might produce type errors at runtime;
  Your function should still correctly return all integer literals appearing in
  the program. See tests for some examples.
|#
(define (extract-ints prog)
  (cond
    [(null? prog) '() ]
    [(list? prog)
     (let ([first-ele (first prog)]
           [rest-list (extract-ints (rest prog))])
       [cond
         [(list? first-ele) (append (extract-ints first-ele) rest-list)]

         [(rational? first-ele) (append (list first-ele) rest-list)]
         [else rest-list]]
       )
     ]
    [else (cond
            [(rational? prog) (list prog)]
            [else '()])
          ]))

(module+ test
  (check-equal? (extract-ints '2) (list 2)) ; Note: '2 == 2, i.e., the ' is unnecessary.
  (check-equal? (extract-ints '(+ 1 2)) (list 1 2))
  (check-equal? (extract-ints '(list 1 2 3 4)) (list 1 2 3 4))
  (check-equal? (extract-ints '(+ (- 9 7) (* 2 8))) (list 9 7 2 8))
  (check-equal? (extract-ints 'ident) (list)))


#|
(replace-324 datum)
  datum: A Racket datum (i.e., quoted expression).

  Return a new datum that is the same as the input, except every occurrence
  of the numeric literal 324 is replaced with 9000.
|#
(define (replace-324 datum)
  (cond
    [(null? datum) list null ]
    [(list? datum)
     (let ([first-ele (first datum)]
           [rest-list (replace-324 (rest datum))])
       [cond
         [(list? first-ele) (append (replace-324 first-ele) rest-list)]
         [(equal? 324 first-ele) (append (list 9000) rest-list)]
         [else (append (list first-ele) rest-list)]]
       )
     ]
    [(equal? 324 datum) 9000]
    [else datum])
  )


(module+ test
  (check-equal? (replace-324 "hello!") "hello!")
  (check-equal? (replace-324 324) 9000)
  (check-equal? (replace-324 '(+ 1 2)) '(+ 1 2))
  (check-equal? (replace-324 '(list 1 2 324 4)) '(list 1 2 9000 4))
  (check-equal? (replace-324 '(+ (- 324 7) (* 2 324))) '(+ (- 9000 7) (* 2 9000))))
