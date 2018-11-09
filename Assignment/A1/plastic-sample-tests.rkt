#lang racket #| ★ CSC324 Fall 2018: Assignment 1 Sample Tests ★ |#
; This file contains some additional sample tests for Assignment 1.
; Please note that these tests are NOT comprehensive, but instead are meant
; to give you a sense of the kinds of tests we'll be running, and to point
; out some key ideas on the assignment.

(require rackunit)
(require "plastic.rkt")


(define defn-Bool '(data Bool = (True) (False)))
(define defn-PairOfBools '(data PairOfBools = (Pair Bool Bool)))

(define defn-pair-and
  '(define pair-and (case-lambda [(Pair (True) (True)) -> (True)]
                                 [(Pair _ _) -> (False)])))

(define definitions (list defn-Bool defn-PairOfBools defn-pair-and))


(define tests
  (test-suite
   "Sample tests"
   (test-equal? "pair-and: True True"
                (run-interpreter (append definitions
                                         '((pair-and (Pair (True) (True))))))
                '(True))

   (test-exn "pair-and: non-exhaustive patterns"
             (regexp "Error: non-exhaustive patterns.")
             (thunk (run-interpreter (append definitions                                          
                                             '((pair-and (True)))))))
   
   (test-exn "Pair: invalid type argument"
             (regexp (format "Error: constructor ~a called incorrectly." 'Pair))
             (thunk (run-interpreter (append definitions
                                             '((Pair (True) (Pair (True) (True))))))))

   ; The following two tests can replace the "unbound name" test in the starter code.
   (test-exn "Error: unbound identifier (lowercase)"
            (regexp (format "Error: identifier ~a could not be found." 'blah))
            (thunk (run-interpreter (append definitions (list 'blah)))))
   (test-exn "Error: unbound identifier (capitalized)"
            (regexp (format "Error: identifier ~a could not be found." 'Blah))
            (thunk (run-interpreter (append definitions (list '(Blah))))))
   
   (test-equal? "Lexical scoping (1)"
                (run-interpreter (list defn-Bool
                                       '(define b (True))
                                       '(define f (case-lambda [-> b]))
                                       '(define g (case-lambda [b -> (f)]))
                                       '(g (False))))
                ; The 'b in (f) should be bound to (True), not (False)
                '(True))

   (test-exn "Lexical scoping (2)"
             (regexp (format "Error: identifier ~a could not be found." 'b))
             ; In this case, the 'b in (f) should not be bound.
             (thunk (run-interpreter (list defn-Bool
                                           '(define f (case-lambda [-> b]))
                                           '(define g (case-lambda [b -> (f)]))
                                           '(g (False))))))))


(require rackunit/text-ui)
(run-tests tests)
