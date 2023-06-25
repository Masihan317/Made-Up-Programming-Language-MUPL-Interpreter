;; Author: Sihan Ma
;; Made Up Programming Language (MUPL) written in Racket

#lang racket

(provide (all-defined-out)) ;; allows tests to be put in a second file

;; definition of structures for MUPL programs
(struct var  (string)      #:transparent) ;; a variable, e.g., (var "foo")
(struct int  (num)         #:transparent) ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)       #:transparent) ;; add two expressions
(struct isgreater (e1 e2)  #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3)    #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body)  #:transparent) ;; a local binding (let var = e in body)
(struct apair   (e1 e2)    #:transparent) ;; make a new pair
(struct first   (e)        #:transparent) ;; get first part of a pair
(struct second  (e)        #:transparent) ;; get second part of a pair
(struct munit   ()         #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)        #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun)  #:transparent)

;; Convert a Racket list to a MUPL list.
(define (racketlist->mupllist lst)
  (if (null? lst)
      (munit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

;; Convert a MUPL list to a Racket list.
(define (mupllist->racketlist lst)
  (if (munit? lst)
      empty
      (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))))

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; evaluate expression under an environment
(define (eval-under-env e env)
  (cond [(var? e) ; var evaluates to what the variable is bound to
         (envlookup env (var-string e))]
        [(add? e) ; add 2 MUPL int
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e] ; int evaluates to itself
        [(munit? e) (munit)] ; munit evaluates to itself
        [(closure? e) e] ; closure evaluates to itself
        [(isgreater? e) ; check which of 2 MUPL int is greater
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1)
                      (int-num v2))
                   (int 1)
                   (int 0))
           (error "MUPL isgreater applied to non-number")))]
        [(ifnz? e) ; check if a MUPL expression is (int 0)
         (let ([v1 (eval-under-env (ifnz-e1 e) env)])
           (if (int? v1)
               (if (zero? (int-num v1))
                   (eval-under-env (ifnz-e3 e) env)
                   (eval-under-env (ifnz-e2 e) env))
               (error "MUPL ifnz applied to non-number")))]
        [(fun? e) (closure env e)] ; MUPL function evaluates to a closure
        [(mlet? e) ; mlet creates a local binding
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e) ; call a MUPL function
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let ([v3 (closure-fun v1)])
                 (eval-under-env (fun-body v3)
                                 (append (closure-env v1)
                                         (list (cons (fun-formal v3) v2)
                                               (let ([v4 (fun-nameopt v3)])
                                                 (if v4
                                                     (cons v4 v1)
                                                     empty))))))
               (error "MUPL call applied to non-closure")))]
        [(apair? e) ; evaluates elements in a MUPL pair respectively
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(first? e) ; MUPL first evaluates its subexpressions
         (let ([v (eval-under-env (first-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL first applied to non-pair")))]
        [(second? e) ; MUPL second evaluates its subexpressions
         (let ([v (eval-under-env (second-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL second applied to non-pair")))]
        [(ismunit? e) ; check if the expression is munit
         (let ([v (eval-under-env (ismunit-e e) env)])
           (if (munit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))])) ; incorrect MUPL expression if does not fall into any other categories

;; evaluate expression on the top level
(define (eval-exp e)
  (eval-under-env e null))

;; executes the second expression if the first is munit, else executes the third expression.
(define (ifmunit e1 e2 e3) (ifnz (ismunit e1) e2 e3))

;; creates cumulative mlet bindings (mlet*). functions like let* in Racket.
(define (mlet* bs e2)
  (if (null? bs)
      e2
      (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))))

;; evaluates the third expression if the first and second are equalm else evaluates the fourth expression.
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifnz (isgreater (var "_x") (var "_y"))
                    e4
                    (ifnz (isgreater (var "_y") (var "_x")) e4 e3)))))

;; curried MUPL filter function that has the same function as Racket filter.
(define mupl-filter
  (fun "f1" "pred"
       (fun "f2" "lst"
            (ifnz (ismunit (var "lst"))
                           (munit)
                           (ifnz (call (var "pred") (first (var "lst")))
                                 (apair (first (var "lst")) (call (call (var "f1") (var "pred")) (second (var "lst"))))
                                 (call (call (var "f1") (var "pred")) (second (var "lst"))))))))

;; MUPL filter function that only keeps values greater than the input integer.
(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun "g" "i"
             (call (var "filter") (fun "filter-i" "n" (isgreater (var "n") (var "i")))))))