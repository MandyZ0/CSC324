#lang racket #| ★ CSC324 Fall 2018: Exercise 8 ★ |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html

Please note that we *are* exporting (and testing) some of the helper
functions we've described in this lab, and not just the main algorithms.
So please make sure to read and follow the specifications carefully for *all*
functions here!

Also, you may not change streams.rkt and stack_choice.rkt (you won't be able
to submit those files; we'll use our own for testing purposes).
★
|#
(require (only-in racket/control prompt))

(provide solve-with-constraints
         with-constraints-helper
         get-constraints
         set->choice

         solve-with-ordered-constraints
         with-ordered-constraints-helper
         initialize-constraints
         sort-constraints
         update-constraints
         )

(require "streams.rkt")
(require "stack_choice.rkt")

;-------------------------------------------------------------------------------
; ★ Sudoku Modeling ★
;-------------------------------------------------------------------------------

; We model a Sudoku board by a Racket *vector* of length 81, representing a 9x9 grid.
; Rows are stored contiguously; for example, the top row of the board is stored in
; the first 9 elements of the vector.
; Racket vectors are an array-based data structure, and provide constant-time
; indexing (unlike Racket lists).
;
; Each vector element is either a between between 1-9, representing a filled cell,
; or the number 0, representing an *empty* cell.
(define board-size 81)
(define 1-9 (list->set (range 1 10)))  ; The possible numbers.
(define (all-nine? s) (set=? s 1-9))   ; Whether the given set contains all numbers 1-9.

(define (blank? board i) (equal? (vector-ref board i) 0))  ; Whether the given cell is blank.


; Utilities for converting between a vector index and the corresponding row, column,
; and 3x3 subsquare in the Sudoku board. This numbering is all 0-indexed.
; The subsquares are numbered starting with 0 in the top-left corner,
; and increase in index first left-to-right, then top-down.
(define (to-column i) (remainder i 9))
(define (to-row i) (quotient i 9))
(define (to-subsquare i)
  (+ (quotient (to-column i) 3)
     (* 3 (quotient (to-row i) 3))))

(define (same-column? i j) (equal? (to-column i) (to-column j)))
(define (same-row? i j) (equal? (to-row i) (to-row j)))
(define (same-subsquare? i j) (equal? (to-subsquare i) (to-subsquare j)))


; Utilities for getting the set of elements in a column/row/subsquare.
; //i is the first element of row or column
(define (column board i)
  (list->set (map (lambda (j) (vector-ref board (+ i (* 9 j)))) (range 9))))

(define (row board j)
  (list->set (map (lambda (i) (vector-ref board (+ i (* 9 j)))) (range 9))))

(define (subsquare board k)
  (let* ([start-row (* 3 (quotient k 3))]
         [start-col (* 3 (remainder k 3))])
    (list->set
     (map (lambda (i)
            (vector-ref board
                        (+ (+ start-col (remainder i 3))
                           (* 9 (+ start-row (quotient i 3))))))
          (range 9)))))


; Return whether a given Sudoku board is completely solved.
; (Review the rules of Sudoku using the link on the exercise handout.)
(define (solved? board)
  (and
   ; Check columns
   (andmap (lambda (col-num) (all-nine? (column board col-num)))
           (range 9))
   ; Check rows
   (andmap (lambda (row-num) (all-nine? (row board row-num)))
           (range 9))
   ; Check subsquares
   (andmap (lambda (sub-num) (all-nine? (subsquare board sub-num)))
           (range 9))))


; Helper function for doing a non-mutating update of a board,
; analogous to list-set.
; This is pretty memory-inefficient, and is a consequence of some limitations
; of our current choice operator when mutation is concerned!
; We've provided an optional argument to turn on logging.
; This may be useful for debugging purposes, or to see how many steps your
; algorithm is taking.
(define (vector-set vec index new-item [logging #f])
  (when logging
    (displayln (format "Index: ~a Choice: ~a" index new-item)))

  (let* ([new-vec (make-vector (vector-length vec))])
    (vector-copy! new-vec 0 vec)
    (vector-set! new-vec index new-item)
    new-vec))


;-------------------------------------------------------------------------------
; ★ A Brute Force Algorithm ★
;-------------------------------------------------------------------------------

; See `brute-force-helper` for details.
(define (solve-brute-force board) (brute-force-helper board 0))


#|
(brute-force-helper board i)
  board: A Sudoku board, in the representation described above.
  i: The current index to consider. Precondition: 0 <= i < 81.

  Considers each board cell one at a time (recurses on `i`).
  Each time it encounters an empty cell, this algorithm creates a *choice point*
  for all 9 numbers that could fill the cell.
  It chooses a number, sets it in the vector, and moves on to the next cell.

  Only when the board is complete does this algorithm check if the board is solved;
  if it isn't, it calls (next) to backtrack to the last choice point, and tries again.
|#
(define (brute-force-helper board i)
  (cond
    ; If there are no more choices to make, check if the puzzle is actually solved.
    [(>= i (vector-length board))
     (if (solved? board)
         board
         (next))]

    ; If the current cell is occupied, move on to the next cell.
    [(not (blank? board i))
     (brute-force-helper board (+ i 1))]

    ; Else, the current cell is empty. Make a new choice!
    [else
     (let* ([choice (-< 1 2 3 4 5 6 7 8 9)]
            [new-board (vector-set board i choice)])
       (brute-force-helper new-board (+ i 1)))]))


;-------------------------------------------------------------------------------
; ★ Task 1: Narrowing choices using constraints ★
;-------------------------------------------------------------------------------

#|
`solve-with-constraints` (and its corresponding helper) are almost exactly the same
as `brute-force`. The only difference is in what choices are made; rather than 
using a static set of choices, the choices for each cell are generated dynamically
based on the current contents of the board.

Complete the two helpers `get-constraints` and `set->choice`, and then modify
`with-constraints-helper` to replace the (-< 1 2 3 4 5 6 7 8 9) expression.
(You can change other things as well, although you shouldn't need to change much.)
|#

;helper function for subtraction in set
(define (subtract xs ys)
  (if (set-empty? ys)
      xs
      (subtract (set-remove xs (set-first ys)) (set-rest ys))))

(define (solve-with-constraints board) (with-constraints-helper board 0))

(define (with-constraints-helper board i)
  ;(displayln (format "constraints helper: index: ~a" i))
  (cond
    ; If there are no more choices to make, check if the puzzle is actually solved.
    [(>= i (vector-length board))
     (if (solved? board)
         
          ;(displayln (format "constraints helper: SLOVED!!"))
          board
         
          ;(displayln (format "constraints helper: NOOOOOT SLOVED!!"))
          (next))]

    ; If the current cell is occupied, move on to the next cell.
    [(not (blank? board i))
     ;(displayln (format "constraints helper: call next index"))
     (with-constraints-helper board (+ i 1))]

    ; Else, the current cell is empty. Make a new choice!
    [else
     ;(displayln (format "constraints helper: get choices of the blank cell"))
     (let* ( [choice (set->choice (get-constraints board i))]
             [new-board (vector-set board i choice)])
       ;(displayln (format "constraints helper: get new-board"))
       (with-constraints-helper new-board (+ i 1)))]))
(define-syntax all-choices
    (syntax-rules ()
      [(all-choices <expr>)
       (begin
         ; Technical hack to add <expr> to the choices stack.
         (prompt (-< (void) <expr>))
         (remaining-choices))]))

  (define (remaining-choices)
    (let* ([v (prompt (next))])
      (if (done? v)
          empty
          (cons v (remaining-choices)))))
#|
(get-constraints board i)
  board: A Sudoku board.
  i: A valid index into `board`. Precondition: i refers to an *empty cell*.

  Returns a *set* of the possible numbers that can fill the empty cell.
  Starts with all 9 possible numbers, and removes the numbers that are
  in the same row, column or subsquare as the given cell.

  Note: You may assume we'll only test this function for the given precondition
  on `i`. In Task 2 you may find it useful to extend the documented behaviour
  for when `i` refers to an occupied cell in the board.
|#
(define (get-constraints puzzle i)
  ;(displayln (format "constraints helper -- call -- get-constrants, index: ~a" i))
  (let* (
         [icolset (column puzzle (to-column i))]
             [irowset (row puzzle (to-row i))]
             [isubsquareset (subsquare puzzle (to-subsquare i))]
             [appeared (set-union icolset irowset isubsquareset)]
             [remained (subtract 1-9 appeared)])
    ;(displayln (format "constraints helper -- end -- get-constrants, get remained: ~a" remained))
    remained))

#|
(set->choice set)
  set: A Racket set

  Returns a choice of an item in set, or calls `next` if the set is empty.
  Hint: the set datatype has functions set-empty?, set-first, and set-rest
  that are analogous to the list functions.
|#
(define (set->choice set)
  ;(displayln (format "constraints helper -- call -- set->choice, set: ~a" set))
  (cond
    [(set-empty? set) (next)]
    [else (-< (set-first set) (set->choice (set-rest set)))]))


;-------------------------------------------------------------------------------
; ★ Task 2: Greedily ordering choices ★
;-------------------------------------------------------------------------------

#|
`solve-with-ordered-constraints` builds on your work in the previous task by
tackling two limitations of the previous approach:

  1. The constraints for each cell are recomputed every time backtracking occurs.
  2. The naive index-order in which the cells are considered may delay applying
     stricter constraints on later cells, leading to more choices (and hence more
     backtracking) made for the early cells.

The main helpers you'll work on here are `initialize-constraints` and
`update-constraints`, which respectively create a list of constraints for all
cells at the start of solving the puzzle, and update these constraints as
choices get made.

We've provided a helper `sort-constraints` for you that you should use to maintain
your list of constraints sorted by non-decreasing number of possibilities.
Your recursive helper will use this order to make choices, which should greatly reduce
the total number of choices made when solving Sudoku boards.
|#
(define (solve-with-ordered-constraints board)
  (with-ordered-constraints-helper board (initialize-constraints board)))


#|
(initialize-constraints board)
  board: A Sudoku board.

  Returns a list of constraints for the blank cells in the given board.
  Represent each constraint as a list of two elements:
    - the first element is the index of the cell
    - the second element is a *set* containing the possible values
      that could fill the cell, using the same constraints as `get-constraints`.

  Assume that the board is solvable, which means that none of the blank cells
  will have an *empty* set of possible values.
|#
(define (initialize-constraints board)
  (initialize-constraints-helper board 0))


(define (initialize-constraints-helper board i)
  (cond
    [(>= i (vector-length board)) '()]
    [(not (blank? board i)) (initialize-constraints-helper board (+ 1 i))]
    [else
        (append (list (list i (get-constraints board i)))
                (initialize-constraints-helper board (+ 1 i)))])
    )


#|
(with-ordered-constraints-helper board constraints)
  board: A Sudoku board.
  constraints: A nested list of the constraints on the remaining blank cells,
               in the format described in `initialize-constraints`.

  Precondition: `constraints` is sorted first by increasing size of the set of
  possible values, and then by increasing index.

  This is the main helper, analogous to the previous two algorithms.
  The main difference here is the second parameter; instead of using an index,
  we use a list of the remaining constraints explicitly.

  Hints:
    - Use the same basic structure as the previous algorithms,
      though the conditions will be different.
    - Remember the basic "first and rest" recursive pattern on lists, and use it here.
|#
(define/match (with-ordered-constraints-helper board constraints)
  ;no constraints
  [(board '())
   (if (solved? board)
       board
       (next))]
  [(board (cons first-constraint rest-constraints))
   (let*([idx (first first-constraint)]
         [consset (second first-constraint)]
         [choice (set->choice consset)]
         [new-board (vector-set board idx choice)]
         )
     ;(displayln (format "constraints helper: continue next constraints"))
      (with-ordered-constraints-helper new-board
        (update-constraints rest-constraints idx choice))
     )]
  )


#|
(update-constraints constraints i choice)
  constraints: A nested list in the form described in `initialize-constraints`.
  i: A valid index into a Sudoku board.
  choice: A integer between 1-9.

  Updates the given constraint list by adding the restriction that cell `i` is
  being given value `choice`. That is, `choice` should be removed from all
  the "possible value" sets for the indexes in the same row, column, or subsquare
  as `i`.

  You may choose to re-sort the constraints here or in the main helper above.

  Important: we strongly recommend calling (next) here if removing `choice`
  produces an empty set for one of the constraints. This corresponds to that
  cell no longer havin any possible values, meaning `choice` is incorrect.
  There are other places you could check for this, but it's probably easiest to
  do it here.
|#
(define (update-constraints constraints i choice)
  (let* ([same-col-idx (remove i (filter (lambda (x) (same-column? x i)) (range 81)))]
         [same-row-idx (remove i (filter (lambda (x) (same-row? x i)) (range 81)))]
         [same-subsquare-idx (remove i (filter (lambda (x) (same-subsquare? x i)) (range 81)))]
         [same-all-idx (append (list i) same-col-idx same-row-idx same-subsquare-idx)]
         ;the list only contain idx updated constraint
        [updated-constraints (foldl (curry-update-constraints-each choice) constraints same-all-idx)]
         )
    updated-constraints)
  )

;helper for each, return updated constraints
(define/match (update-constraints-each constraints choice idx)
  [('() choice idx) '()]
  [((cons first-constraint rest-constraints) choice idx)
    (let*([considx (first first-constraint)]
          [consset (second first-constraint)])
      ;(displayln (format "update-constraints-each: choice: ~a, idx: ~a, considx: ~a, consset: ~a" choice idx considx consset))
       (if (equal? idx considx)
          ;we need to update
          (let ([newconsset (set-remove consset choice)])
            (if (set-empty? newconsset)
                (next)
                (append (list(list considx newconsset)) (update-constraints-each rest-constraints choice idx))))
          ;don't need to update
          (append  (list first-constraint) (update-constraints-each rest-constraints choice idx))
          )
      
    )]
  )

(define (curry-update-constraints-each choice)
  (lambda (idx constraints)
    (update-constraints-each constraints choice idx))
  )
;for checking:
(define harder-constraints
  (list
 (list 0 (set 5 4))
 (list 1 (set 5 7 4 8))
 (list 3 (set 9 4))
 (list 5 (set 1 7 4))
 (list 7 (set 5 9 7 8))
 (list 8 (set 5 7))
 (list 10 (set 7 4 8 2 6))
 (list 11 (set 7 4))
 (list 13 (set 7 4))
 (list 15 (set 7 8))
 (list 16 (set 7 8 2))
 (list 18 (set 5 2))
 (list 19 (set 5 7 2))
 (list 22 (set 9 7))
 (list 25 (set 5 9 3 7 2))
 (list 26 (set 5 3 7 2))
 (list 27 (set 5 3 4))
 (list 28 (set 5 3 4))
 (list 31 (set 5 3 4 6))
 (list 34 (set 5 3 7 4 6))
 (list 35 (set 5 3 7 4 6))
 (list 37 (set 1 5 9 3 4 2))
 (list 38 (set 9 4))
 (list 39 (set 5 9 4))
 (list 40 (set 5 9 3 4 6))
 (list 41 (set 4))
 (list 42 (set 1))
 (list 43 (set 1 5 3 4 6))
 (list 45 (set 1 5 3 4))
 (list 46 (set 1 5 9 3 4))
 (list 49 (set 5 9 3 4))
 (list 52 (set 1 5 3 4))
 (list 53 (set 5 3 4))
 (list 54 (set 1 3 4))
 (list 55 (set 1 3 7 4))
 (list 58 (set 7 4 8))
 (list 61 (set 1 7 4 8))
 (list 62 (set 7 4))
 (list 64 (set 1 7 4 6))
 (list 65 (set 7 4))
 (list 67 (set 5 7 4))
 (list 69 (set 1 7))
 (list 70 (set 1 7 4 6))
 (list 72 (set 4 6))
 (list 73 (set 9 7 4 6))
 (list 75 (set 4))
 (list 77 (set 7 4))
 (list 79 (set 7 4 8 2 6))
 (list 80 (set 7 4 2 6))))

#|
(sort-constraints constraints)
  constraints: A nested list in the form described in `initialize-constraints`.

  Sorts the list of constraints first by increasing size of the set of possible values,
  and then by increasing index.

  This function is given to you; please don't change it!
|#
(define (sort-constraints constraints)
  (sort constraints
        (lambda (a b)
          (or (< (set-count (second a)) (set-count (second b)))
              (and (= (set-count (second a)) (set-count (second b)))
                   (< (first a) (first b)))))))

;-------------------------------------------------------------------------------
; ★ Demos ★
;-------------------------------------------------------------------------------
#|
This section includes some code for running your algorithms on actual Sudoku boards.

You can safely ignore all of this code, expect the invocations of the algorithms at
the bottom, which start off commented-out.

We took some puzzles from https://projecteuler.net/problem=96, but added our own
(very easy) puzzle at the front. 
|#
; A puzzle file, and a function to parse it into separate puzzles.
  (define in (open-input-file "p096_sudoku.txt" #:mode 'text))

  ; Get the next puzzle from the file.
  ; Note that this is written in an imperative style; as we'll discuss later
  ; in the course, it's much harder to get away from this style when doing I/O
  ; computations.
  (define (get-next-puzzle)
    ; Check for the header line "Grid XX". If eof is found, we've reached the end of the file.
    (if (eof-object? (read-line in))
        (void)
        (let* ([nested-list-form
                (map
                 (lambda (_)
                   ; This processes a single line, converting it from a 9-letter string into a list of integers.
                   (map (lambda (c) (- (char->integer c) (char->integer #\0)))
                        (string->list (read-line in))))
                 (range 9))])
          (list->vector (apply append nested-list-form)))))

  ; A stream of puzzles.
  (define (puzzle-stream)
    (let* ([puzzle (get-next-puzzle)])
      (if (void? puzzle)
          s-null
          (s-cons puzzle (puzzle-stream)))))

  
  (define all-puzzles (stream->list (puzzle-stream)))
  (define easy (first all-puzzles))
  (define harder (second all-puzzles))

(module+ main
  )

; Run the brute force algorithm on the "easy" puzzle.
; Note that we call reset-choices! so that it doesn't interfere with subsequent choices.
#;(module+ main
    (solve-brute-force easy)
    (reset-choices!))

; Run the Task 1 algorithm on the "easy" puzzle.
#;(module+ main
    (solve-with-constraints easy)
    (reset-choices!))

; Run the Task 1 algorithm on the "harder" puzzle.
; Constrast these two!
#;(module+ main
    (solve-with-constraints harder)
    (reset-choices!))

; Run the Task 2 algorithm on the "harder" puzzle.
#;(module+ main
    (solve-with-ordered-constraints harder)
    (reset-choices!))

; Can you run your algorithm on all of the puzzles in the file?
(module+ main
    (define all-solutions
      (map (lambda (p)
             (reset-choices!)
             (solve-with-ordered-constraints p))
           all-puzzles))
    ; This line should return #t.
    (andmap solved? all-solutions))
