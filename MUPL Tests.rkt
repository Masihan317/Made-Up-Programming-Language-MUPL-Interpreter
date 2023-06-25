;; Author: Sihan Ma
;; Tests for Made Up Programming Language (MUPL) written in Racket

#lang racket

(require "MUPL.rkt")

; This file uses Racket's unit-testing framework.
(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (racketlist->mupllist '(10 x)) (apair 10 (apair 'x (munit)))
                 "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair 10 (apair 'x (munit)))) (list 10 'x)
                 "mupllist->racketlist test")

   (check-equal? (eval-exp (int 10)) (int 10)
                 "MUPL int test")
   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4)
                 "MUPL add test")
   (check-equal? (eval-exp (mlet "x" (int 17) (var "x"))) (int 17)
                 "MUPL mlet test && MUPL var test")
   (check-equal? (eval-exp (fun "id" "x" (var "x"))) (closure '() (fun "id" "x" (var "x")))
                 "MUPL fun test")
   (check-equal? (eval-exp (closure '() (fun "id" "x" (var "x")))) (closure '() (fun "id" "x" (var "x")))
                 "MUPL closure test")
   (check-equal? (eval-exp (call (fun "id" "x" (var "x")) (int 5))) (int 5)
                 "MUPL call test")
   (check-equal? (eval-exp (isgreater (int 10) (int 5))) (int 1)
                 "MUPL isgreater test")
   (check-equal? (eval-exp (ifnz (isgreater (int 10) (int 5)) (int 7) (int 8))) (int 7)
                 "MUPL ifnz test")
   (check-equal? (eval-exp (first (apair (int 2) (int 3)))) (int 2)
                 "MUPL apair test && MUPL first test")
   (check-equal? (eval-exp (ismunit (second (apair (int 3) (munit))))) (int 1)
                 "MUPL second test && MUPL ismunit test")

   (check-equal? (eval-exp (ifmunit (int 10) (int 99) (int 100))) (int 100)
                 "MUPL ifmunit test")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 7)) (cons "x" (int 9))) (var "x"))) (int 9)
                 "MUPL mlet* test")
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4)
                 "MUPL ifeq test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")
   (check-exn (lambda (x) (string=? (exn-message x) "MUPL isgreater applied to non-number"))
              (lambda () (eval-exp (isgreater (int 2) (munit))))
              "isgreater bad argument")
   (check-exn (lambda (x) (string=? (exn-message x) "MUPL ifnz applied to non-number"))
              (lambda () (eval-exp (ifnz (munit) (int 1) (int 0))))
              "ifnz bad argument")
   (check-exn (lambda (x) (string=? (exn-message x) "MUPL call applied to non-closure"))
              (lambda () (eval-exp (call (munit) (int 1))))
              "call bad argument")
   (check-exn (lambda (x) (string=? (exn-message x) "MUPL first applied to non-pair"))
              (lambda () (eval-exp (first (int 5))))
              "first bad argument")
   (check-exn (lambda (x) (string=? (exn-message x) "MUPL second applied to non-pair"))
              (lambda () (eval-exp (second (int 6))))
              "second bad argument")

   ; Tests for Part IV
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   ))

(require rackunit/text-ui)

;; runs the test
(run-tests tests)