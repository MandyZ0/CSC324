#lang racket #| CSC324 Fall 2018: Lab 8 |#
(require "stack_choice.rkt")

;-------------------------------------------------------------------------------
; ★ Review: arithmetic expression generator from lecture ★
;-------------------------------------------------------------------------------
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

(define atom (thunk (-< 1 2 3 4)))
(define plus-rule (lambda (e1 e2) (list '+ e1 e2)))
(define times-rule (lambda (e1 e2) (list '* e1 e2)))
(define rule (thunk (-< plus-rule times-rule)))


#|
(expression-of-rank k)
  k: A non-negative integer

  Returns a (choice of) binary arithmetic expression of rank k.
|#
(define/match (expression-of-rank k)
  [(0) (atom)]
  [(1) ((rule) (expression-of-rank (- k 1)) (expression-of-rank (- k 1)))]
  [(_)
   (-<
    ; Combinations of k-1 and 0-(k-2).
    ((rule)
     (expression-of-rank (- k 1))
     (expression-of-rank (num-between 0 (- k 2))))
    ; Combinations of 0-(k-2) and k-1.
    ((rule)
     (expression-of-rank (num-between 0 (- k 2)))
     (expression-of-rank (- k 1)))
    ; Combinations of k-1 and k-1.
    ((rule)
     (expression-of-rank (- k 1))
     (expression-of-rank (- k 1))))])


;-------------------------------------------------------------------------------
; ★ Task 1: Generalizing expression generation ★
;-------------------------------------------------------------------------------
#|
(expression-of-rank-gen grammar k)
  grammar: A list containing two thunks, where the first thunk returns a choice
           of an atom in the grammar, and the second returns a choice of a rule
           in the grammar.
  k: A non-negative integer.

  Returns a (choice of) binary expression of rank k from the grammar.
|#
(define/match (expression-of-rank-gen grammar k)
    [((cons atom (cons rule '())) 0) (atom)]
  [((cons atom (cons rule '())) 1) ((rule) (expression-of-rank-gen grammar (- k 1)) (expression-of-rank-gen grammar (- k 1)))]
  [((cons atom (cons rule '())) _)
   (-<
    ; Combinations of k-1 and 0-(k-2).
    ((rule)
     (expression-of-rank-gen grammar (- k 1))
     (expression-of-rank-gen grammar (num-between 0 (- k 2))))
    ; Combinations of 0-(k-2) and k-1.
    ((rule)
     (expression-of-rank-gen grammar (num-between 0 (- k 2)))
     (expression-of-rank-gen grammar (- k 1)))
    ; Combinations of k-1 and k-1.
    ((rule)
     (expression-of-rank-gen grammar (- k 1))
     (expression-of-rank-gen grammar (- k 1))))])


; Your function should work for the following input grammars.
(define arithmetic-grammar (list atom rule))
(define life-choices-grammar
  (list
   (thunk
     (-< "cats" "dogs" "birds" "love" "terror" "hunger"))
   (thunk
     (-< (lambda (e1 e2) (format "~a and ~a!" e1 e2))
         (lambda (e1 e2) (format "~a or ~a?" e1 e2))))))


;-------------------------------------------------------------------------------
; ★ Task 2: Making change ★
;-------------------------------------------------------------------------------
#|
(make-change n)
  n: A non-negative integer.

  Returns a choice of a list of 1's and 5's whose sum is n. The returned list
  should be in *non-increasing order*, i.e., all 5's should appear before all 1's.

  Note: we strongly recommend writing a helper function that takes a number and
  returns a list of 1's of that length. This will prepare you to do the recursive
  thinking required to generalize `make-change` later in the lab.

  You might want to lookup the "build-list" function.
|#

(define/match (list-of-one n)
  [(0) '()]
  [(_) (cons 1 (list-of-one (- n 1)))])

;helper funcition like 'rule' for combining
(define (combine-with-five)
  (lambda (lst) (append (list 5) lst)))

(define (make-change n)
  (cond
    [(< n 5) (-<(list-of-one n))]
    [else (-< ((combine-with-five) (make-change (- n 5)))
              (list-of-one n))]))

  (require (only-in racket/control prompt))

  ; Helper to access all choices
  (define-syntax all-choices
    (syntax-rules ()
      [(all-choices <expr>)
       (begin
         ; Technical hack to add <expr> to the choices stack.
         (prompt (-< (void) <expr>))
         (remaining-choices))]))

  ; Like get-choices, except grabs *all* remaining choices.
  (define (remaining-choices)
    (let* ([v (prompt (next))])
      (if (done? v)
          empty
          (cons v (remaining-choices)))))

(module+ test
  (require rackunit)


  (test-equal? "make-change 0"
               (list->set (all-choices (make-change 0)))
               (set empty))
  (test-equal? "make-change 1"
               (list->set (all-choices (make-change 1)))
               (set (list 1)))
  (test-equal? "make-change 5"
               (list->set (all-choices (make-change 5)))
               (set (list 5) (list 1 1 1 1 1)))
  (test-equal? "make-change 13"
               (list->set (all-choices (make-change 13)))
               (set (list 5 5 1 1 1)
                    (list 5 1 1 1 1 1 1 1 1)
                    (list 1 1 1 1 1 1 1 1 1 1 1 1 1))))


#|
(make-change-gen coins n)
  coins: A list of distinct positive integers, sorted in decreasing order.
  n: An integer.

  Returns a choice of a list of numbers in `coins` whose sum is `n`.
  The list should be sorted in non-increasing order.
  As before, coin values may be used more than once.

  If no possible combinations in `coins` sums to n (including when n is negative),
  *call (next)*. (This is useful inside recursive calls.)
  But return an *empty list* if n = 0. These are different cases!

//coins: list of numbers

  Notes:
    1. We have started the pattern-matching for you, and strongly recommend building
       off of these patterns.
    2. You might need to have the output choices come out in a different order
       than in `make-change`. That's fine!
|#

(define (list-of-coin coin n)
  (cond
    [(equal? n 0) '()]
    [else (cons coin (list-of-coin coin (- n coin)))]))

(define (combine-with-coin coin)
  (lambda (lst) (append (list coin) lst)))

(define/match (make-change-gen coins n)
  ; If n = 0, return an empty list.
  [(_ 0) '()]

  ; If there are no more coins to choose from, backtrack by calling (next).
  [((list) _) (next)]

  ; If there's just one coin value, think about what to do here!
  [((list coin) _)
   (cond [(equal? (remainder n coin) 0) (-<(list-of-coin coin n))]
         [else (next)])] ;here has some problem of whether put next in the funciton or outside

  ; If there's more than one coin value, choose one of:
  ;   - a combination of (rest coins) whose sum is n
  ;   - a combination of coins whose sum is (- n (first coins)), together with
  ;     an occurrence of (first coins)
  [((cons first-coin rest-coins) _)
   (cond
     [(< n first-coin) (make-change-gen rest-coins n)]
     [else (-< ((combine-with-coin first-coin) (make-change-gen coins (- n first-coin)))
               (make-change-gen rest-coins n))])])


(module+ test
  (test-equal? "make-change-gen (10 3) 13"
               (list->set (all-choices (make-change-gen (list 10 3) 13)))
               (set (list 10 3)))

  (test-equal? "make-change (5 1) 13"
               (list->set (all-choices (make-change-gen (list 5 1) 13)))
               (set (list 5 5 1 1 1)
                    (list 5 1 1 1 1 1 1 1 1)
                    (list 1 1 1 1 1 1 1 1 1 1 1 1 1)))

  ; A note about this test. It may seem weird that the expected output is (set)
  ; and not (set 'done). The reason is that `all-choices` returns `empty` when
  ; there are no more choices (see its implementation above).
  (test-equal? "no combinations possible"
               (list->set (all-choices (make-change-gen (list 10 3) 17)))
               (set)))
