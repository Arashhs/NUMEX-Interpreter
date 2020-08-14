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
  (cond[(munit? xs) null]
       [ #t (cons (apair-e1 xs)(numexlist->racketlist (apair-e2 xs)))]))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  	[(not (list? env)) (error "enviornment is not a list")]
        [(not (string? str)) (error "str is not a string")]
        [(eq? (caar env) str)(cdar env)]
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
         (cond [(integer? (num-int e)) e]
               [#t (error "NUMEX num must contain an integer")])]

        [(bool? e) ;; Evaluating bool
         (cond [(boolean? (bool-boolean e)) e]
               [#t (error "NUMEX bool must contain a boolean")])]

        [(munit? e) ;; Evaluating munit
         (munit)]

        [ (closure? e) e]

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
               (num (quotient (num-int v1) 
                       (num-int v2)))
               (error "NUMEX division applied to non-number")))]

        [(andalso? e) ;; Evaluating andalso
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           (if (and (bool? v1)
                    (eq? (bool-boolean v1) #f)) v1
                                                (let ([v2 (eval-under-env (andalso-e2 e) env)])
                                                  (if (bool? v2) v2
                                                      (error "At least one of the arguments of andalso is not a bool expression")))))]

        [(orelse? e) ;; Evaluating orelse
         (let ([v1 (eval-under-env (orelse-e1 e) env)])
           (if (and (bool? v1)
                    (eq? (bool-boolean v1) #t)) v1
                                                (let ([v2 (eval-under-env (orelse-e2 e) env)])
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
                   (bool #f))))]
    ;;    (error "the arguments types are not both num or both bool")

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
                           (cons (cons (with-s e) v1)env) ;; Extended environment
                           ))]

        [(lam? e) ;; Evaluating lam
         (closure env e)]

        [(apply? e) ;; Evaluating apply
         (let ([funClosure (eval-under-env (apply-funexp e) env)]
               [actual (eval-under-env (apply-actual e) env)])
           (if (closure? funClosure)
               (let ([closEnv (closure-env funClosure)]                
                     [funDec (closure-f funClosure)])
                 (if (lam? funDec)
                     (let ([funName (lam-nameopt funDec)]
                           [funFormal (lam-formal funDec)])
                       (eval-under-env (lam-body funDec) (cons (cons funFormal actual)(cons (cons funName funClosure)closEnv))))
                     (error "Closure's function is not in the right shape (lam)"))) 
               (if (lam? funClosure)
                   (let ([funName (lam-nameopt funClosure)]
                           [funFormal (lam-formal funClosure)])
                       (eval-under-env (lam-body funClosure) (cons (cons funFormal actual)(cons (cons funName funClosure) env))))
                   (error "Function doesn't evaluate to closure"))))]

        

        [(apair? e) ;; Evaluating apair
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        [(1st? e) ;; Evaluating 1st
         (let ([v1 (eval-under-env (1st-e1 e) env)])
           (if (apair? v1) (apair-e1 v1)
               (error "Argument of NUMEX 1st must evaluate to apair")))]

        [(2nd? e) ;; Evaluating 2nd
         (let ([v1 (eval-under-env (2nd-e1 e) env)])
           (if (apair? v1) (apair-e2 v1)
               (error "Argument of NUMEX 2nd must evaluate to apair")))]

        [(ismunit? e) ;; Evaluating ismunit
         (let ([v1 (eval-under-env (ismunit-e e) env)])
           (if (munit? v1) (bool #t)
               (bool #f)))]

        [(letrec? e) ;; Evaluating letrec
         (let ([e1 (letrec-e1 e)]
               [s1 (letrec-s1 e)]
               [e2 (letrec-e2 e)]
               [s2 (letrec-s2 e)]
               [e3 (letrec-e3 e)]
               [s3 (letrec-s3 e)]
               [e4 (letrec-e4 e)])
           (if (and (string? s1)(string? s2)(string? s3))
               (eval-under-env e4 (cons (cons s1 e1)(cons (cons s2 e2)(cons (cons s3 e3) env))))
               (error "First, third, and fifth arguments of NUMEX letrec must be string")))]

        [(queue? e) ;; Evaluating queue
         (let ([exp (eval-under-env (queue-e e) env)]
               [q (queue-q e)])
           (cond [(munit? q) (queue exp q)]
                 [(queue? q) (queue exp q)]
                 [#t (error "Second argument of NUMEX queue must evaluate to either a queue or munit")]))]

        [(enqueue? e) ;; Evaluating enqueue
         (let ([exp (eval-under-env (enqueue-e e) env)]
               [q (enqueue-q e)])
           (cond [(queue? q) (queue exp q)]
                 [#t (error "Second argument of NUMEX enqueue must evaluate to a queue")]))]
         
         [(dequeue? e) ;; Evaluating dequeue
         (let ([q (dequeue-q e)])
           (cond [(queue? q) (let ([restq (queue-q q)]
                                   [firstq (queue-e q)])
                               (cond [(munit? restq) (munit)]
                                     [#t (queue firstq (eval-under-env (dequeue restq) env))]))]
                 [#t (error "Only argument of NUMEX dequeue must evaluate to a queue")]))]

         [(extract? e) ;; Evaluating extract
         (let ([q (extract-q e)])
           (cond [(queue? q) (let ([restq (queue-q q)]
                                   [firstq (queue-e q)])
                               (cond [(munit? restq) firstq]
                                     [#t (eval-under-env (extract restq) env)]))]
                 [#t (error "Only argument of NUMEX extract must evaluate to a queue")]))]
         

        
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3)
  (cnd (ismunit e1) e2 e3))

(define (with* bs e2)
  (cond [(null? (cdr bs)) (with (caar bs) (cdar bs) e2)]
        [#t (with (caar bs) (cdar bs) (with* (cdr bs) e2))]))

(define (ifneq e1 e2 e3 e4)
  (cnd (iseq e1 e2) e4 e3))

;; Problem 4

(define numex-filter (lam "function" "filterFun" ;; Returns the function "function" which takes a function "filterFun" as argument
                          (lam "numexFilter" "numexList" 
                               (cnd (ismunit (var "numexList")) (munit)
                                    (ifnzero (apply (var "filterFun") (1st (var "numexList")))
                                              (apair (apply (var "filterFun") (1st (var "numexList")))
                                                     (apply (var "numexFilter") (2nd (var "numexList"))))
                                              (apply (var "numexFilter") (2nd (var "numexList"))))))))



(define numex-all-gt
  (with "numex-filter" numex-filter
        (lam "function" "i" (apply (var "numex-filter") (lam "greater than" "number" (ifleq (var "number") (var "i") (num 0)(var "number")))))))

        