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
(struct ifleq  (e1 e2 e3 e4)  #:transparent)  ;; if e1 <= e2 then e3 else e4 

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
  (cond[(ismunit xs) (null)]
       [ #t (cons (apair-e1 xs)(numexlist->racketlist (apair-e2 xs)))]))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  		"CHANGE" 
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