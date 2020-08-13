;; PL Project - Spring 2020
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool (boolean) #:transparent) ;; a boolean constant
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus  (e1 e2)  #:transparent)  ;; subtraction of two expressions 
(struct mult  (e1 e2)  #:transparent)  ;; multiplication of two expressions 
(struct div  (e1 e2)  #:transparent)  ;; division of two expressions 
(struct neg  (e1)  #:transparent)  ;; negation of a expression 
(struct andalso  (e1 e2)  #:transparent)  ;; logical conjunction of two expression 
(struct orelse  (e1 e2)  #:transparent)  ;; logical disjunction of two expression 
(struct cnd  (e1 e2 e3)  #:transparent)  ;; if e1 then e2 else e3 
(struct iseq  (e1 e2)  #:transparent)  ;; comparison
(struct ifnzero  (e1 e2 e3)  #:transparent)  ;; if e1 != zero then e2 else e3 
(struct ifleq  (e1 e2 e3 e4)  #:transparent)  ;; if e1 > e2 then e4 else e3

(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application

(struct with (s e1 e2) #:transparent) ;;  a let expression where the value of e1 is bound to s in e2
(struct apair (e1 e2) #:transparent) ;; pair constructor
(struct 1st (e1) #:transparent) ;; the first part of a pair
(struct 2nd (e1) #:transparent) ;; the second part of a pair

(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 

(struct letrec (s1 e1 s2 e2 s3 e3 e4) #:transparent) ;; a letrec expression for recursive definitions
(struct queue (e q) #:transparent) ;; it holds several expressions
(struct enqueue (e q) #:transparent) ;; it enqueues e into q
(struct dequeue (q) #:transparent) ;; it dequeues q
(struct extract (q) #:transparent) ;; it returns queue's top element

;; Problem 1

(define (racketlist->numexlist xs)
  (cond[(null? xs) (munit)]
      [ #t (apair (car xs) (racketlist->numexlist (cdr xs)))]))

(define (numexlist->racketlist xs)
  (cond[(munit? xs) (null)]
       [ #t (cons (apair-e1 xs)(numexlist->racketlist (apair-e2 xs)))]))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  	[(not (list? env)) (error "enviornment is not a list")]
        [(not (string? str)) (error "str is not a string")]
        [#t (envlookup (cdr env) str )]
		)
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(num? e) ;; Evaluating num
         (cond [(integer? num-int e)(e)]
               [#t (error "NUMEX num must contain an integer")])]

        [(bool? e) ;; Evaluating bool
         (cond [(boolean? bool-boolean e)(e)]
               [#t (error "NUMEX bool must contain a boolean")])]

        [(munit? e) ;; Evaluating munit
         (munit)]

        [(minus? e) ;; Evaluating subtraction
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]

        [(mult? e) ;; Evaluating multiplication
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]

        [(div? e) ;; Evaluating division
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (floor (/ (num-int v1) 
                       (num-int v2))))
               (error "NUMEX division applied to non-number")))]

        [(andalso? e) ;; Evaluating andalso
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           (if (and (bool? v1)
                    (eq? (bool-boolean v1) #f)) v1
                                                (let ([v2 (eval-under-env (andalso-e2 e) env)])
                                                  (if (bool? v2) v2
                                                      (error "At least one of the arguments of andalso is not a bool expression")))))]

        [(neg? e) ;; Evaluating neg
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (if (bool? v1) (bool (not (bool-boolean v1)))
               (if (num? v1) (num (- (num-int v1)))
                (error "NUMEX neg applied to non-number and non-boolean"))))]

        [(cnd? e) ;; Evaluating cnd
         (let ([v1 (eval-under-env (cnd-e1 e) env)])
           (if (bool? v1)
                    (if (eq? (bool-boolean v1) #t) (eval-under-env (cnd-e2 e) env)
                        (eval-under-env (cnd-e3 e) env))
                    (error "First argument of NUMEX cnd must be a bool expression")))]

        [(iseq? e) ;; Evaluating iseq
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (if (and (bool? v1)
                    (bool? v2))
               (bool (eq? (bool-boolean v1)(bool-boolean v2)))
               (if (and (num? v1)
                        (num? v2))
                   (bool (eq? (num-int v1)(num-int v2)))
                   (error "the arguments types are not both num or both bool"))))]

        [(ifnzero? e) ;; Evaluating ifnzero
         (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
           (if (num? v1)
                    (if (not (eq? (num-int v1) 0)) (eval-under-env (ifnzero-e2 e) env)
                        (eval-under-env (ifnzero-e3 e) env))
                    (error "First argument of NUMEX ifnzero must evaluate to a num")))]

        [(ifleq? e) ;; Evaluating ifleq
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
                    (if (> (num-int v1)(num-int v2)) (eval-under-env (ifleq-e4 e) env)
                        (eval-under-env (ifleq-e3 e) env))
                    (error "First two arguments of NUMEX ifleq must evaluate to a num")))]

        [(with? e) ;; Evaluating with
         (let ([v1 (eval-under-env (with-e1 e) env)])
           (eval-under-env (with-e2 e)
                           (cons (cons (with-s e) v1)(env)) ;; Extended environment
                           ))]
         

        
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (with* bs e2) "CHANGE")

(define (ifneq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define numex-filter "CHANGE")

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))