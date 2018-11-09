#lang racket

(define-syntax my-class
  (syntax-rules (method)
    [; Pattern (the code we want to write)
     (my-class <class-name> (<attr> ...)
       (method (<method-name> <param> ...)
               <body>) ...)

     ; Template (the code we want to run)
     ; We use begin to sequence two definitions.
     (begin
       (define class__dict__
         ; Dictionary of methods. Shared among all instances.
         (make-immutable-hash
          (list (cons (quote <method-name>)
                      (lambda (<param> ...) <body>)) ...)))
       (define (<class-name> <attr> ...)
         ; Note the use of letrec so that we can refer to the object lambda inside it.
         (letrec
             ([self__dict__
               (make-immutable-hash
                (list (cons (quote <attr>) <attr>) ...))]
              [me (lambda (msg)
                    (cond
                      ; Do a lookup first at the instance dict, then the class dict.
                      [(hash-has-key? self__dict__ msg)
                       (hash-ref self__dict__ msg)]
                      [(hash-has-key? class__dict__ msg)
                       (fix-first me (hash-ref class__dict__ msg))]
                      [else ((attribute-error <class-name> msg))]))])
           me))
       )]
    ))


; From a previous exercise ;)
(define (fix-first x f)
  (lambda args (apply f (cons x args))))

; Return a function that raises an attribute error (with an appropriate message).
(define (attribute-error object attr)
  (lambda ()
    (error (format "~a has no attribute ~a." object attr))))


; Uses of the my-class macro.
(my-class Point (x y)
  (method (size self)
          (sqrt (+ (* (self 'x) (self 'x)) (* (self 'y) (self 'y)))))
  (method (same-radius self other)
          (equal? ((self 'size)) ((other 'size)))))

(my-class Person (name age likes-chocolate))


(define p (Point 2 3))
(p 'x)  ; p.x
(p 'y)  ; p.y
((p 'size))  ; p.size()
