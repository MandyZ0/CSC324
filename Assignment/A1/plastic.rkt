#lang racket #| ★ CSC324 Fall 2018: Assignment 1 ★ |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html ★

The assignment handout can be found at
https://www.cs.toronto.edu/~david/csc324/assignments/a1/handout.html
|#
; You may use the following imports, but you may not add any other imports.
(require racket/hash)
(require "plastic-errors.rkt")

(provide run-interpreter)


;-------------------------------------------------------------------------------
; ★ Main interpreter function ★
;-------------------------------------------------------------------------------
#|
(run-interpreter prog) 
  prog: A datum representing a Plastic program.
    Consult the assignment handout for the assumptions you can make about prog.

  The main interpreter function.

  Returns the value of the program, or raises an error reporting one of the
  kinds of semantic errors you're responsible for on this assignment.

  While we've provided some structure for this function already, you are free
  to modify the implementation as you see fit (just don't change the public
  behaviour).
|#

(define (run-interpreter prog)
  ; Split up prog into the definitions and final expression to evaluate.
  (let* ([definitions (drop-right prog 1)]       ; drop the last element: expression
         [expr (last prog)]                      ; the last element is expression
         [envs (build-envs definitions)]         ; build environment based on definitions
         [type-env (first envs)]                 ; there are type env and value env
         [value-env (second envs)])
    (interpret type-env value-env expr)))

(define (data-identifier? expr)
  (and (symbol? expr)
       (char-lower-case? (string-ref (symbol->string expr) 0))))

;all identifiers used for type names and
;constructor names must start with a capital letter; 
(define (type-identifier? expr)
  (and (symbol? expr)
       (char-upper-case? (string-ref (symbol->string expr) 0))))

;-------------------------------------------------------------------------------
; ★ Parts 1 and 2: Building the environments ★
;-------------------------------------------------------------------------------
#|
(build-envs definitions)
  definitions: A list of datums representing either type declarations or name bindings.
    Note: as specified by the assignment grammar, all type declarations will
    appear before any name bindings.

  Returns two hash tables (list type-env value-env), where the first hash table stores
  type information, and the second stores name bindings.

  We've started this off as two separate hash tables; however, because of the naming restrictions
  we impose on the language, it's possible to store everything in just one dictionary,
  as long as you're careful.
|#

(define (curryfoldone typename)
  (lambda (oneconstru inithash)
    ;    (cond
    ; [(equal? 'Cons (first oneconstru)) (hash-set inithash 'Cons (rest oneconstru))]
    (hash-set inithash (first oneconstru) (cons typename (rest oneconstru))))
  );)

;((curryfoldone 'SomeBools) '(TwoBools Bool Bool) (make-immutable-hash))

;new logic: it's in the typehashlist, filter not. so list only contain the element not in the oldtypehash
(define (unboundhelp lst)
  (lambda (onedef initlist)
    (append (filter-not (lambda (type-name) (list? (member type-name
                                                           lst)))
                        onedef) initlist))
  )

(define (get-case-pattern rule)
  ; get the pattern portion of a case-lambda rule
  ; e.g. (get-case-pattern '(a b -> c)) returns '(a b)
  (drop-right rule 2))
(define (get-case-body rule)
  ; get the body portion of a case-lambda rule
  ; e.g. (get-case-pattern '(a b -> c)) returns 'c
  (last rule))

;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;important helper function for taking out all the parentheses!!!
(define (extract lst)
  (cond
    [(null? lst) (list)]
    [else
     (let ([first-ele (first lst)]
           [rest-lists (extract (rest lst))])
       (cond
         [(list? first-ele) (append (extract first-ele) rest-lists)]
         [else (append (list first-ele) rest-lists)]))]
    )
  )


;(map (lambda (lst) (first lst)) (hash-values oldtypehash)): get all the typevalues
(define/match (newtypehash oldhashlist typedef)
  ;type:
  [(oldhashlist (cons 'data (cons typename (cons '= consdefs))))
   ;original code
   ;(foldl (curryfoldone typename) oldtypehash consdefs)
   ;the version going to update:
   (cond
     [(equal? (findf (lambda (arg) (equal? arg typename))
                     (map (lambda (lst) (first lst)) (hash-values (first oldhashlist)))) typename)
      (report-error 'duplicate-name typename)]
     ;if it's not recursive
     [(and (not (member 'Cons (extract consdefs))) (not (null? (foldl (unboundhelp (map (lambda (lst) (first lst))
                                                                                        (hash-values (first oldhashlist))))
                                                                      (list) (map (lambda (lst) (rest lst)) consdefs)))))
      (report-error 'unbound-name (first (foldl (unboundhelp (map (lambda (lst) (first lst)) (hash-values (first oldhashlist)))) (list) (map (lambda (lst) (rest lst)) consdefs))))]
     [else (foldl (curryfoldone typename) (first oldhashlist) (reverse consdefs))])
   ]



  ;name binding: (define x (True))
  ;I NEED TO FIND WAY TO ACCESS EACH ELEMENT
  ;(define f (case-lambda []))
  [(oldhashlist (cons 'define (cons id (cons expr '()))))
   ;original code
   ;(foldl (curryfoldone typename) (first oldhashlist) consdefs)
   ;the version going to update:
   (cond
     [(equal? (findf (lambda (arg) (equal? arg id))
                     (map (lambda (lst) (first lst)) (hash-values (first oldhashlist)))) id)
      (report-error 'duplicate-name id)]
     
     ;'unbound-name should be thrown in the case of any unbound name, including unbound identifiers, type-names, and constructor-names
     ;很有问题!!!!! 应该是UNboundhelp
     ;change expression!!!! (append expr)
     ;unboundhelp output a list of unbound
     ;UNSOLVE: DEFINE RECURSIVE TYPE

     ;case lambda: create closure!!!!!

     ;(case-lambda [(Pair (True) (True)) -> (True)]
     ;[(Pair _ _) -> (False)])
     [(case-lambda? expr)
      (let ([rules (rest expr)])
        (hash-set (second oldhashlist) id (hash-set (second oldhashlist) id (foldl (lambda (onerule inithash)
                                                                                     (let([pattern (get-case-pattern onerule)]
                                                                                          [body (get-case-body onerule)])
                                                                                       (hash-set inithash pattern body))) (make-immutable-hash) rules)))
        )]
     ;fold: input: list
     ;output: hash-table pattern-> rule
     ;then I need to hash it to the value-env
     [(not (null? ((unboundhelp (hash-keys (first oldhashlist))) (extract expr) (list))))
      (report-error 'unbound-name ((unboundhelp (hash-keys (first oldhashlist))) (extract expr) (list)))]
     ;(append (list (first '(Pair (False) (True)))) (filter-map first (rest '(Pair (False) (True)))))
     ;(data TwoBool = (Pair Bool Bools)) -> Bools is invalid!
     ; it's invalid: if any paras haven't appeared before -> unbound-name
     ;so every para in consdefs
     
     [else
      (cond
        [(equal? (findf (lambda (arg) (equal? arg id))
                        (hash-keys (second oldhashlist))) id)
         (report-error 'duplicate-name id)]
        [else (hash-set (second oldhashlist)
                        id expr)])])
   ]
  
  
  )



;I'M ABLE TO return a hash map based on one whole type definition!!
;next step is to fold all type definitions!
;(newtypehash fmap '(data Boolean = (True) (False)))
; #hash((True. (Boolean '())),
;       (False. (Boolean '()))  )

(define (case-lambda? v)
  (and (list? v)
       (not (empty? v))
       (equal? (first v) 'case-lambda)))
;we need filter for all typedef & iddef
;input: definitions (typedef & iddef)
(define (buildenvfold onedef inithashlist)
  (cond
    [(type-identifier? (second onedef)) (list (newtypehash inithashlist onedef) (second inithashlist))]
    ;value-environment
    [(data-identifier? (second onedef)) (list (first inithashlist) (newtypehash inithashlist onedef))]
    ))

;MAIN BUILD-ENVS function:
(define (build-envs definitions)
  (foldl buildenvfold (list (make-immutable-hash) (make-immutable-hash)) definitions)
  )


#|
Two errors can occur:
unbound name & duplicate name
|#
;------------------------------------------------------------------------------
; ★ Parts 3-4: The actual interpreter ★
;-------------------------------------------------------------------------------
#|
(interpret type-env value-env expr)
  type-env: type environment (a hash table)
  value-env: name bindings (a hash table)
  expr: the expression to evaluate


  Returns the result of evaluating expr in the given type and name environment.
|#
(define (interpret type-env value-env expr)
  (cond
    ;func call
    [(and (list? expr) (hash-has-key? value-env (first expr)))
     (let([patterns (hash-keys (hash-ref (hash-ref value-env (first expr)) (first expr)))]
          [values (rest expr)]) 
       (println"patterns")
       (println(list patterns))
       (println"values")
       (println(list values))
       (cond
         ;null para
         [(empty? (first patterns))
          (let
              ([body (hash-ref (hash-ref (hash-ref value-env (first expr)) (first expr)) '())])
            (cond
              [(data-identifier? body) (interpret type-env (hash-ref value-env (first expr)) body)]
              [else body]))]
         ;can't find
         [(not (pattern-match patterns values)) (report-error 'non-exhaustive-patterns)]
         ;_ _ or all match: 
         [(hash-empty? (second (pattern-match patterns values)))
          (let ([body (hash-ref (hash-ref (hash-ref value-env (first expr)) (first expr)) (first (pattern-match patterns values)))])
            (cond
              [(data-identifier? body) (interpret type-env (hash-ref value-env (first expr)) body)]
              [else body]))]
         ;there's matching in pattern
         [else
          (let ([body (hash-ref (hash-ref (hash-ref value-env (first expr)) (first expr)) (first (pattern-match patterns values)))]
                [pattern-match-table (second (pattern-match patterns values))])
           ; (println"body")
           ; (println(list body))
            (cond
              [(hash-has-key? pattern-match-table body)
               (cond [(data-identifier? (hash-ref pattern-match-table body)) (interpret type-env (hash-ref value-env (first expr)) (hash-ref pattern-match-table body))]
                     [else (hash-ref pattern-match-table body)]
                     )]
              [(data-identifier? body)
             ;  (println"body is dataidentifier")
             ;  (println(list body))
               (interpret type-env (hash-ref value-env (first expr)) body)]
              ;body is func call (f)
              [(and (list? body) (data-identifier? (first body)))
            ;   (println(hash-ref value-env (first body)))
               (interpret type-env (hash-ref value-env (first expr)) body)]
              [else body]))]
         ))]
    ;call id
    [(hash-has-key? value-env expr) (hash-ref value-env expr)]
    ;a constructor is called
    [(list? expr)
     (cond
       [(not (hash-has-key? type-env (first expr))) (report-error 'unbound-name (first expr))]
       [(andmap (lambda (onearg oneexprarg) (equal? onearg (first (hash-ref type-env (first oneexprarg))))) (rest (hash-ref type-env (first expr))) (rest expr)) expr]
       [else (report-error 'constructor-type-error (first expr))]
     
       ) ]
    [else (report-error 'unbound-name expr)]
    ))

;here will also happen unbound identifier!!!!


;-------------------------------------------------------------------------------
; ★ Part 3: Implementing pattern-matching ★
;-------------------------------------------------------------------------------
#|
(pattern-match patterns values)
   patterns: one or more patterns
   values: one or more values

Matching an underscore pattern should produce an empty hash table (no bindings).
Matching a lower-case identifier name to value v should produce the hash table corresponding to {name: v}.
Matching a constructor pattern should produce the union of all bindings produced by matching its sub-patterns.

   Returns a hash table containing the name bindings on a successful match,
   and #f otherwise.
 it should produce a hash table binding each pattern variable to the matching value
++++++++++what it means for a pattern to match a value.+++++++++++++
patterns   '(((True) (True)) (_ _))
one pattern: ((True) (True))
|#
;input: onepattern values
;output: boolean
(define (each-argument-match-constru args values inithash)
  (println"run each-arg-match-constru")
  (match args
    [(list) ;(println"0 argument")
            inithash] ; 0 argument
    [(cons onearg '()) ; 1 argument (_) or (y)
     (print"onearg") (println(list onearg))
     (print"value") (println(list values))
     (cond
       [(equal? onearg '_) inithash]
       [(data-identifier? onearg)
       ; (println"first values")
       ; (println(list (first values)))
        (hash-set inithash onearg (first values))]
       ;(True)
       [(and (list? onearg) (list? (first values)) (equal? (length onearg) (length (first values))) (equal? (first onearg) (first (first values))))
        (cond
          [(empty? (rest onearg)) inithash]
          [else #f]
          )]
       )]
    ;multiple args
    [(cons firstarg restargs)
     (println"multiple args")
     (print"firstarg  ") (println(list firstarg))
     (print"restarges ") (println(list restargs))
     (print"values ") (println(list values))
     (let ([rest-hash (each-argument-match-constru restargs (rest values) inithash)])
     (cond
       [(equal? firstarg '_) rest-hash]
       [(data-identifier? firstarg)
       ; (println"first values")
       ; (println(list (first values)))
        (hash-set rest-hash firstarg (first values))]
       ;(True)
       [(and (list? firstarg) (list? (first values)) (equal? (length firstarg) (length (first values))) (equal? (first firstarg) (first (first values))))
        (cond
          [(empty? (rest firstarg)) rest-hash]
          [else (each-argument-match-constru (rest firstarg) (rest (first values)) inithash)]
          )]
       ))

     ;(each-argument-match-constru (rest onearg) (rest values) inithash)
     ]
    ))


(define (multi-para-match arg value inithash)
  (println"multi para match")
 (match arg
   [(cons onearg '()) ; 1 argument (_) or (y)
     (print"multi-para-match-onearg") (println(list onearg))
     (print"value") (println(list value))
     (cond
       [(equal? onearg '_)
        (println"multi-para-match-onearg   '_")
        inithash]
       [(data-identifier? onearg)
        (println"multi-para-match-onearg   id")
       ; (println"first values")
       ; (println(list (first values)))
        (hash-set inithash onearg (first value))]
       ;(True)
       [(and (list? onearg) (list? (first value)) (equal? (length onearg) (length (first value))) (equal? (first onearg) (first (first value))))
        (println"multi-para-match-onearg   constructor")
        (cond
          [(empty? (rest onearg))
           (println"multi-para-match-onearg   constructor empty rest onearg")
           inithash]
          [else
           (println"multi-para-match-onearg   constructor continue match constru")
           (each-argument-match-constru (rest onearg) (rest (first value)) inithash)
           ]
          )]
       [else
        (println"multi-para-match-onearg   FALSE")
        #f]
       )]
   [(cons firstarg rest-args) ;several arguments ((True) (True))
    (println"multi-para-match several arguments")
    (print"firstarg ") (println(list firstarg))
    (print"value ")  (println(list value))
     (let ([rest-hash (multi-para-match rest-args (rest value) inithash)])
      (cond
        [(not rest-hash) #f]
        [(equal? firstarg '_)
                 (println"multi-para-match-several-arguments   '_")
                rest-hash]
               [(data-identifier? firstarg)
                (println"multi-para-match-several-arguments   identifier")
                (hash-set rest-hash firstarg (first value))]
                                            ;(hash-set (each-argument-match-constru rest-args (rest values) inithash) firstarg (first values))
                                            
               [(and (list? firstarg) (list? (first value)) (equal? (length firstarg) (length (first value))) (equal? (first firstarg) (first(first value))))
                (println"multi-para-match-several-arguments  constructor")
                (cond
                  [(empty? (rest firstarg)) ;(println"empty rest firstarg")
                   (println"multi-para-match-several-arguments  constructor   empty rest firstarg")
                                            rest-hash]
                  [else
                   (println"multi-para-match-several-arguments  constructor continue match rest args")
                   (each-argument-match-constru (rest firstarg) (rest (first value)) rest-hash)])
                ]
               
               [else
                (println"multi-para-match-several-arguments  FALSE")
                #f]
                 
               ))]
   )
  )

(define (each-argument-match arg value inithash)
  (println"each-arg-match")
  (print"arg ")
  (println(list arg))
  (print"value ")
  (println(list value))
  (cond
    [(equal? (length arg) 1)
      (cond [(equal? (first arg) '_)
     (println"each-arg-match '_")
     inithash]
    [(and (data-identifier? (first arg)) (equal? (length (first arg)) 1))
    (println"each-arg-match data-id")
     (hash-set inithash (first arg) (first value))]
    [(and (not (list? (first (first arg)))) (list? (first arg)) (list?(first value)) (equal? (length (first arg)) (length (first value))) (equal? (first (first arg)) (first (first value))))
     (println"each-arg-match constructor match")
     (cond
       [(empty? (rest (first arg))) ;(println"empty rest firstarg")
                   (println"each-arg-match  constructor   empty rest arg")
                                            inithash]
     [else
      (println"each-arg-match  constructor   continue rest arg")
      (each-argument-match-constru  (rest (first arg)) (rest (first value)) inithash)])
     ])]
     

    [(not (equal? (length arg) 1))
     (println"each-argument-match multi para match ")
     (print"arg ")
     (println(list arg))
     (print"value ")
     (println (list value))
     (multi-para-match arg value inithash)
     ]
    [else #f]
    ))



(define (pattern-match patterns values)
  (println"run pattern-match")
  (match patterns
    [(cons one-pattern '())
     (println"match one pattern")
     (cond
       [(each-argument-match one-pattern values (make-immutable-hash)) (list one-pattern (each-argument-match one-pattern values (make-immutable-hash)))]
       [else #f])
     ]
    [(cons first-pattern rests)
     (println"match multi patterns")
     (cond
       [(each-argument-match first-pattern values (make-immutable-hash)) (list first-pattern (each-argument-match first-pattern values (make-immutable-hash)))]
       [else
     ;   (println"list rests")
       ; (println(list rests))
        (pattern-match rests values)])
     ]))
;(define match? (each-argument-match '(Pair (True) y) '(Pair (False) (True)) (make-immutable-hash)))


;-------------------------------------------------------------------------------
; ★ Part 4: Runtime type-checking ★
;-------------------------------------------------------------------------------
; Please put your main work for Part 4 here. (Makes it easier for your TAs to find.)


;-------------------------------------------------------------------------------
; ★ General helpers (feel free to add to these here, or below) ★
;-------------------------------------------------------------------------------
(define myhash (build-envs '((data Bool = (True) (False))
                             (data Null = (Empty)) 
                             (data TwoBools = (Pair Bool Bool) (One Bool))
                             (data ThreeBools = (Three Bool Bool Bool))
                             (data ListOfBools = (Null) (Cons Bool ListOfBools))
                             (define b (True))
                             (define f (case-lambda [-> b]))
                             (define g (case-lambda [b -> (f)]))
                             )))
;(println"pattern match example")
;(hash-ref (hash-ref (second myhash) 'and) 'and)
;(define pattern-match-example (pattern-match '((Pair (True) (True)) (Pair _ _)) '((Pair (True) (True)))))

;(println"run interpreter")
(run-interpreter '((data Bool = (True) (False))
                   (data Null = (Empty)) 
                   (data TwoBools = (Pair Bool Bool) (One Bool))
                   (data ThreeBools = (Three Bool Bool Bool))
                   (data ListOfBools = (Null) (Bool))
                   (define x (False))
                   (define b (False))
                   (define f (case-lambda [-> x]))
                   (define g (case-lambda [b -> (f)]))
                   (define pair-and (case-lambda [(Pair (True) (True)) -> (True)]
                                                 [(Pair _ _) -> (False)]))
                   (define and (case-lambda [(True) (True) -> (True)]
                                            [_ _ -> (False)]))
                   (g (True))
                   ))




;-------------------------------------------------------------------------------
; ★ Sample tests (please add to these!) ★
;-------------------------------------------------------------------------------
(module+ test
  ; This first part creates some definition datums, useful for testing purposes.

  ;'(data Void = (V)))
  ; [data]: declare a new type
  ; [type-name]: Void   **start with a capital letter
  ;[constructor-definition]: (V) creating values of that type.
  ;**Each constructor definition consists of a name (for the constructor)
  ;followed by zero or more type names, which specify the arguments to
  ;that constructor. 

  
  ; A type named Void with one nullary constructor V.
  ;**constructors can be nullary (i.e., take zero arguments)
  (define defn-Void '(data Void = (V)))

  ; The familiar boolean type, which has exactly two values.
  (define defn-Bool '(data Bool = (True) (False)))

  ; An *enumeration type* like Bool is a type whose constructors are all nullary.
  ; Here's another example of an enumeration type.
  (define defn-Day '(data Day = (Mon) (Tue) (Wed) (Thu) (Fri) (Sat) (Sun)))

  ; Here is a type representing a pair of booleans.
  (define defn-PairOfBools '(data PairOfBools = (Pair Bool Bool)))

  ; Here is a complex type representing either zero, one, or two booleans.
  (define defn-SomeBools
    '(data SomeBools =
           (NoBools)
           (OneBool Bool)
           (TwoBools Bool Bool)))
  ;Each constructor definition consists of a name (for the constructor)
  ;followed by zero or more type names, which specify the arguments to
  ;that constructor. 

  ; Here is a recursive "list of booleans" type.
  (define defn-ListOfBools
    '(data ListOfBools =
           (Empty)
           (Cons Bool ListOfBools)))
  
  ; ======================
  ; We use the define keyword to bind global names to values
  #|Eagerly evaluated:
1.Evaluate the <expr> in the current environment.
2.Update the current environment to bind the LOWERCASE-ID
to the value of <expr>.

This means that every binding is evaluated,
even if the name is not used anywhere in the program.
Like type declarations, name bindings are evaluated in top-down order,
and attempting to evaluate a name before it is bound
is also a semantic error.
|#
  (define defn-identity
    '(define identity (case-lambda [x -> x]))) ; [x(pattern) -> x(expr)]
  (define defn-not
    '(define not (case-lambda [(True) -> (False)]
                              [(False) -> (True)])))
  (define defn-and       ; define action(and) in plastic language
    '(define and (case-lambda [(True) (True) -> (True)]
                              [_ _ -> (False)])))
  (define defn-pair-and  
    '(define pair-and (case-lambda [(Pair (True) (True)) -> (True)]
                                   [(Pair _ _) -> (False)]))))
#|
Expressions:
There are only three kinds of expressions in the Plastic language:
identifiers, function calls, and case-lambdas.

<expr> = LOWERCASE-ID                   # identifier
       | <case-lambda>                  # pattern-matching function
            this is an expression representing an
            anonymous pattern-matching function,
            similar to Racket’s match-lambda*.
       | ( <expr> <expr> ... )          # case-lambda call
        function call   arguments       #   (the first <expr> is the function expression,
                                        #   then come zero or more function arguments)
       | ( CAPITALIZED-ID <expr> ... )  # constructor call
|#

; Sample tests actually start here. Uncomment when ready.
(module+ test
    (require rackunit)
    (test-equal? "Single void"
                 (run-interpreter (list defn-Void '(V)))
                 '(V))
    (test-equal? "Void with lookup"
                 (run-interpreter (list defn-Void '(define x (V)) 'x))
                 '(V))
    
    )