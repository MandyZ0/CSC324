#lang racket
(define (person name age likes)
  (lambda (msg)
    (cond
      [(equal? msg 'name) name]
      [(equal? msg 'age) age]
      [(equal? msg 'likes) likes]
      [(equal? msg 'greet)
       (lambda (other)
         (format "Hello, ~a! I'm ~a, nice to meet you!" (other 'name) name))]
      [(equal? msg 'can-vote?) (>= age 18)])))

(define-syntax list-comp
  (syntax-rules (: <- if)
    [(list-comp <out-expr> : (<id1> <- <lst-expr1>) (<id2> <- <lst-expr2>) ... if <filter>)
     (map (lambda (<id1> <id2> ...) <out-expr>) <lst-expr1>  <lst-expr2> ...)]))

; (list-comp (+ x y) : (x <- (list 1 2 3)) (y <- (list 2 3 4)))

; From a previous exercise ;)
(define (fix-first x f)
  (lambda args (apply f (cons x args))))

(define (attribute-error object attr)
  (lambda ()
    (error (format "~a has no attribute ~a." object attr))))


(define-syntax my-class
  (syntax-rules (method private inheritance)
        [; Pattern (the code we want to write), we assume that this first one is only for base class, 16:54, now can be universal
     (my-class <class-name> (<attr> ...)
       (method ((<method-name> <param> ...)
               <body>) ...)
       )

     ; Template (the code we want to run)
     ; We use begin to sequence two definitions.
     (begin
       (define (<class-name> <attr> ...)
         ; Note the use of letrec so that we can refer to the object lambda inside it.
         (letrec
             ([self__dict__
               (make-immutable-hash
                (list (cons (quote <attr>) <attr>) ...))]
              [class__dict__                                          
         ; Dictionary of methods. Shared among all instances.
         (make-immutable-hash
          (list (cons (quote <method-name>)
                      (lambda (<param> ...) <body>)) ...))]
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
    [; Pattern (the code we want to write)
     (my-class <class-name> (<attr> ...)
       (private ((<private-name> <priparam> ...)
               <pribody>) ...)
       (method ((<method-name> <param> ...)
               <body>) ...)
       )

     ; Template (the code we want to run)
     ; We use begin to sequence two definitions.
     (begin
       
       (define (<class-name> <attr> ...)
         ; Note the use of letrec so that we can refer to the object lambda inside it.
         (letrec
             ([self__dict__
               (make-immutable-hash
                (list (cons (quote <attr>) <attr>) ...))]
              [<private-name> (lambda (<priparam> ...) <pribody>)]... ; move private method and class__dict__ inside letrec, also can call self
              [class__dict__                                          
         ; Dictionary of methods. Shared among all instances.
         (make-immutable-hash
          (list (cons (quote <method-name>)
                      (lambda (<param> ...) <body>)) ...))]
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
    [; Pattern (the code we want to write) // inheritance
     (my-class <class-name> ((<attr> ...) (<base-attr> ...)) (inheritance <base-class>)
       (private ((<private-name> <priparam> ...)
               <pribody>) ...)
       (method ((<method-name> <param> ...)
               <body>) ...)
       )

     ; Template (the code we want to run)
     ; We use begin to sequence two definitions.
     (begin
       (define (<class-name> <attr> ... <base-attr> ...)
         
         ; Note the use of letrec so that we can refer to the object lambda inside it.
         (letrec
             ([self__dict__
               (make-immutable-hash
                (list (cons (quote <attr>) <attr>) ...))]
              [<private-name> (lambda (<priparam> ...) <pribody>)] ... ; move private method and class__dict__ inside letrec, also can call self
              [base (<base-class> <base-attr> ...)]
              [class__dict__                                          
         ; Dictionary of methods. Shared among all instances.
         (make-immutable-hash
          (list (cons (quote <method-name>)
                      (lambda (<param> ...) <body>)) ...))]
              [me (lambda (msg)
                    (cond
                      ; Do a lookup first at the instance dict, then the class dict.

                      [(hash-has-key? self__dict__ msg)
                       (hash-ref self__dict__ msg)]
                      [(hash-has-key? class__dict__ msg)
                       (fix-first me (hash-ref class__dict__ msg))]
                      [(base msg)  ; now the problem becomes like animal is a class, cannot call sel__dict
                       (base msg)] 
                    
                      [else ((attribute-error <class-name> msg))]))])
           me))
       )]
    ))

(my-class animal (gender weight)
          (private ((grow-weight! self growth) (+ weight growth)))
          (method ((get-gender self) (self 'gender))
                  ((get-weight self growth) (grow-weight! self growth))))

(my-class person2 ((age height) (gender weight)) (inheritance animal)
          (private ((After-n-years self years) (+ (self 'age) years))
                   ((Grow-n-centimeter grows) (+ height grows)))
          (method ((Difference self other year) (- (After-n-years self year) (other 'age)))
                  ((Difference-Height self other grow) (- (Grow-n-centimeter grow) (other 'height)))))

(define p (person2 20 180 'female 50))

(require "../gitrepo/324/CSC324/Lecture_Example/week7/stack_choice.rkt")

;exercise 2.17: why it's wrong to generate all binary strings of length 5?
;what means by using thunks to fix it?
(let* ([char (-< #\0 #\1)])
  (string char char char char char))
(define/match (list->c lst)
  [((cons x '())) (-< x)]
  [((cons x xs)) (-< x (list->c xs))]
  )

(define/match (list->choice lst1 lst2)
  [('() '()) 'done]
  [((cons x xs) '()) (-< x (list->choice xs '()))]
  [('() (cons x xs)) (-< x (list->choice '() xs))]
  [((cons x xs) (cons y ys)) (-< x y (list->choice xs ys))])


(define (permutation? lst1 lst2)
  (let ([newlst (filter (lambda (x) (member x lst2)) lst1)])
    (if (equal? lst1 newlst)
        #t
        #f)))

(define/match (map-< f lst)
  [(f (cons x '())) (f (-< x))]
  [(f (cons x xs))
   (f (list->c lst))
  ])