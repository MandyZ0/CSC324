#lang racket #| ★ CSC324 Fall 2018: Assignment 1 ★ |#
#|
Support module for reporting semantic errors in Plastic programs.
You should read through this file to make sure you know how to call
`report-error` appropriately, but may not make any changes to this
file. (You aren't submitting it; we're providing our own version for testing.)
|#
(provide report-error)

;-------------------------------------------------------------------------------
; ★ Error reporting (don't change this) ★
;-------------------------------------------------------------------------------
(define error-strings
  #hash(
        ; Requires the identifier (a symbol).
        (duplicate-name . "Error: identifier ~a was used twice in a definition.")

        ; Requires the identifier (a symbol).
        (unbound-name . "Error: identifier ~a could not be found.")

        ; Requires the name of the constructor (a symbol).
        (constructor-type-error . "Error: constructor ~a called incorrectly.")

        (not-a-function . "Error: trying to call ~a, but it is not a function.")
        (non-exhaustive-patterns . "Error: non-exhaustive patterns.")
        ))


#|
(report-error error-type . args)
  error-type:
    A symbol corresponding to one of the semantic errors your interpreter should handle.
    See the assignment handout or the error-strings hash table for the valid keys.
  args:
    Additional arguments passed into the format string for the error. These are documented
    individually for each error type; see the error-strings hash table for details.

  Raises an error with a message generated from `error-strings`.
  Pay attention to the required `args` for each error string!

  For example, reporting a 'duplicate-name error requires passing in the name that's
  been duplicated, as a symbol. For example: (report-error 'duplicate-name 'x)
|#
(define (report-error error-type . args)
  (error (apply format (cons (hash-ref error-strings error-type) args))))
