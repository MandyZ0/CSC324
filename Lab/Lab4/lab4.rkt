#lang racket #| CSC324 Fall 2018: Lab 4 |#

;-------------------------------------------------------------------------------
; ★ Task 1: Closures as objects ★
;-------------------------------------------------------------------------------
#|
(Point x y)
  x: an integer (representing the x-coordinate of the point)
  y: an integer (representing the y-coordinate of the point)

  Returns a function representing the *point* (x, y).
  Read the given code carefully to see how the returned function expects to
  be called, and experiment in the interpreter!

|#

; here it helps you define a Point class already
;input: x y
;output: return a funtion (takes one parameter), we assigned the function
;to another name p, call (p 'x) ('x is the parameter), then it will return x
;task1-4: A method is an attribute whose type is a function. ex: scale
(define (Point x y)
  (lambda (attr)
    (cond
      [(equal? attr 'x) x]
      [(equal? attr 'y) y]
      [(equal? attr 'scale) (lambda (s)
                              (Point (* s x) (* s y)))]
      [else (error (format "Point has no attribute ~a." attr))])))
;task1-0: Use Point to define a value representing the point (1, -3).
(define p (Point 1 -3))

;task1-1: Write a function that takes in one Point value
;and returns the sum of its two coordinates.
(define (SumOfPoint pt)
  (+ (pt 'x) (pt 'y)))

;task1-2: Write a function that takes in two Point values
;and returns the distance between them.
(define (Dis pt)
  (abs (- (p 'x) (p 'y))))

;task1-3: Write a function that takes in a positive integer n
;and returns a list of points with coordinates
;(0, 0), (1, 1), ..., (n-1, n-1).
(define (listOfPoint n)
  (cond
    [(equal? n 1) (list (Point (- n 1) (- n 1)))]
    [else (append (listOfPoint (- n 1)) (list (Point (- n 1) (- n 1))))]))



;-------------------------------------------------------------------------------
; ★ Task 2: The attribute __dict__ ★
;-------------------------------------------------------------------------------
(define (PointHash x y)
  (let* ([__dict__
          (make-immutable-hash
           (list (cons 'x x)
                 (cons 'y y)
                 (cons 'scale
                       (lambda (factor)
                         (PointHash (* x factor) (* y factor))))
            )
           )
          ])
    
    (define (attribute-error object attr)
  (lambda ()
    (error (format "~a has no attribute ~a." object attr))))
;what you're going to return: (intput: a message you want to look up in the dictionary)
    (lambda (msg)
      (hash-ref __dict__ msg
                (attribute-error 'Point msg))
      )))
