#lang racket
(require csc151)
(require racket/match)
(require racket/undefined)
(require rackunit)
;; CSC 151.02 (Fall 2020, Term 2)
;;Project 4
;;   https://rebelsky.cs.grinnell.edu/Courses/CSC151/2020Fa2/code/labs/...
;; Authors: Liam Walsh
;; Date: December 2nd 2020 
;; Acknowledgements:
;;   Sam R guided me through part 1 with list and cons pattern when I was stuck. And helped me with part 2 recusrison

;;; (match-cons-helper pattern value)->booleon
;;; pattern: pattern?
;;; value: value?
;;; return true  if  (cons head tail) matches (head (car value) and tail matches (cdr value)
(define match-cons-helper  
  (lambda (pattern value)
    (if (or (null? pattern)(null? value))
        #f
        (and (match? (car pattern) (car value)) (match? (list-ref pattern 1)(cdr value))))))

;;; (match-cons? pattern value)->Boolean?
;;; pattern:pattern?
;;; value:value?
;;; returns true if value follows (cons p1 p2) pattern.
(define match-cons?
  (lambda (pattern value)
    (and  (list? pattern) (list? value)
          (equal? 'cons (car pattern))
          (not (null? value))
          (not(empty-list-pattern pattern value))
          (match-cons-helper (cdr pattern) value))))

;;; (empty-list-pattern pattern value)->boolean?
;;; pattern: any pattern? 
;;; value:any value?
;;; returns true if value matches empty list pattern
;;; acknowslgement code addopted from project description page.
(define empty-list-pattern
  (lambda (pattern value)
    (and (pair? pattern) ; It's a list
         (null? (cdr pattern)) ; Containing one element
         (eq? (car pattern) 'list)  
         (null? value))))

;;; (list-match-helper pattern value)->boolean?
;;; pattern: list?
;;; value: list?
;;; will check if each part of pattern matches each value.
(define list-match-helper
  (lambda (pattern value)
    (if  (null? value) 
         #t
         (and (match?  (car pattern) (car value)) (list-match-helper (cdr pattern) (cdr value))))))

;;; (list-match? pattern value)->Boolean?
;;; pattern: pattern?
;;; value: value?, a value
;;; Returns true  if value matches list pattern
(define list-match?
  (lambda (pattern value)
    (and  (list? pattern)
          (list? value)
          (equal? 'list (car pattern))
          (not(empty-list-pattern pattern value))
          (equal? (- (length pattern) 1) (length value))
          (list-match-helper (cdr pattern) value))))

;;; (match? Pattern value)->boolean?
;;; Pattern:any Pattern?
;;; value:any value?
;;; returns true if inputted Pattern matches inputted value.
(define match?
  (lambda (pattern value)
    (cond 
      ; Variables match everything
      [(symbol? pattern)
       #t]
      ;; check if intergaer matching problem. two integers equal each other
      [(and (integer? pattern) (integer? value) (eq? pattern value))
       #t]
      ;; if both inputs are strings and strings match eachother. than this must be matching string pattern
      [(and (string? pattern) (string? value) (string=? pattern value))
       #t]
      [ (list-match? pattern value)
        #t]
      ;; empty (list) mathces '()
      [  (empty-list-pattern  pattern value)
         #t] 
      ;; cons case
      [ (match-cons? pattern value)
        #t]    
      ; Everything else currently fails to match
      [else
       #f])))
; Tests of variable patterns. 
(check-true (match? 'x 1) 
            "variables: match variable to integer")
(check-true (match? 'x "one") 
            "variables: match variable to string")
(check-true (match? 'x '()) 
            "variables: match variable to empty list")
(check-true (match? 'x '(1)) 
            "variables: match variable to nonempty list")
; Tests of number patterns. 
(check-true (match? 1 1) 
            "integers: match integer to same integer")
(check-false (match? 1 -1)
             "integers: match integer to other integer")
(check-false (match? 1 "1")
             "integers: match integer to string")
(check-false (match? 1 '())
             "integers: match integer to empty list")
(check-false (match? 1 '(1))
             "integers: match integer to singleton list")
; Common tests of string patterns. 
(check-true (match? "Hello" "Hello")
            "strings: match string to itself")
(check-false (match? "Hello" "hello")
             "strings: match string to alternate capitalization")
(check-false (match? "Hello" "Goodbye")
             "strings: match string to different string")
(check-false (match? "Hello" "")
             "strings: match string to empty string")
(check-false (match? "Hello" 1)
             "strings: match string to integer")
; Edge cases of string patterns. 
(check-true (match? "" "")
            "string/edge: match empty string to itself")
(check-false (match? "" "hello")
             "string/edge: match empty string to nonempty string")
(check-false (match? "" '())
             "string/edge: match empty string to empty list")
(check-false (match? "" 0)
             "string/edge: match empty string to zero")
(check-false (match? "" '(""))
             "string/edge: match empty string to singleto list")
; Empty list, v1. 
(check-true (match? '(list) '())
            "empty/1: match empty list to empty list")
(check-true (match? '(list) '())
            "empty/1: match empty list to empty list")
(check-false (match? '(list) (list 1))
             "empty/1: match empty list to singleton list created with list")
(check-false (match? '(list) (cons 1 null))
             "empty/1: match empty list to singleton list created with cons")
(check-false (match? '(list) '(1))
             "empty/1: match empty list to singleton list created with tick")
; Cons cells with set first element.
(check-true (match? '(cons 1 xs) (list 1))
            "cons/1: match (cons 1 xs) and singleton list of 1")
(check-true (match? '(cons 1 xs) (list 1 2 3))
            "cons/1: match (cons 1 xs) and longer list starting with 1")
(check-false (match? '(cons 1 xs) '())
             "cons/1: match (cons 1 xs) and empty list")
(check-false (match? '(cons 1 xs) (list 2))
             "cons/1: match (cons 1 xs) and list starting with 2")
(check-false (match? '(cons 1 xs) 1)
             "cons/1: match (cons 1 xs) and integer")
(check-false (match? '(cons 1 xs) "1")
             "cons/1: match (cons 1 xs) and string")
; Two-element lists created with `(list "a" y)`
(check-true (match? '(list "a" y) (list "a" "b"))
            "pair/3:a")
(check-true (match? '(list "a" y) (list "a" 2))
            "pair/3:b")
(check-false (match? '(list "a" y) (list "b" "a"))
             "pair/3:c")
(check-false (match? '(list "a" y) (list "a" "b" "c"))
             "pair/3:d")
(check-false (match? '(list "a" y) (list "a"))
             "pair/3:d")
(check-false (match? '(list "a" y) (list))
             "pair/3:e")
; Singleton lists created with '(cons x (list))
(check-true (match? '(cons x (list)) (list 1))
            "singleton/1: match (cons x (list)) with (list 1)")
(check-true (match? '(cons x (list)) (list "hello"))
            "singleton/1: match (cons x (list)) with string list")
(check-true (match? '(cons x (list)) (cons 1 (list)))
            "singleton/1: match (cons x (list)) with cons list")
(check-false (match? '(cons x (list)) null)
             "singleton/1: match (cons x (list)) with empty list")
(check-false (match? '(cons x (list)) "hello")
             "singleton/1: match (cons x (list)) with string")
(check-false (match? '(cons x (list)) (list 1 2))
             "singleton/1: match (cons x (list)) with two-element list")
; Two-element lists created with `(list x y)`
(check-true (match? '(list x y) (list 1 2))
            "pair/1:a")
(check-true (match? '(list x y) (list "hello" "goodbye"))
            "pair/1:b")
(check-false (match? '(list x y) (list "hello" "and" "goodbye"))
             "pair/1:c")
(check-false (match? '(list x y) (list "hello"))
             "pair/1:d")
(check-false (match? '(list x y) (list))
             "pair/1:e")
; Two-element lists created with `(list "a" y)`
(check-true (match? '(list "a" y) (list "a" "b"))
            "pair/3:a")
(check-true (match? '(list "a" y) (list "a" 2))
            "pair/3:b")
(check-false (match? '(list "a" y) (list "b" "a"))
             "pair/3:c")
(check-false (match? '(list "a" y) (list "a" "b" "c"))
             "pair/3:d")
(check-false (match? '(list "a" y) (list "a"))
             "pair/3:d")
(check-false (match? '(list "a" y) (list))
             "pair/3:e")
; Some edgier things, often with deeper nesting.   
(check-true (match? '(list (list (list (list x y)))) 
                    (list (list (list (list 1 2)))))
            "edgier/1:a")
(check-false (match? '(list (list (list (list x y)))) 
                     (list (list (list (list 1 2 3)))))
             "edgier/1:b")
(check-false (match? '(list (list (list (list x y)))) 
                     (list (list (list (list 3)))))
             "edgier/1:c")
(check-false (match? '(list (list (list (list x y)))) 
                     (list (list (list 3))))
             "edgier/1:c")

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; part 2
;;;;;;;;;;;;;;;;;;;;;;;

;;; (find-match pattern value)->value?
;;; pattern: a pattern?
;;; value: a value?
;;; if a value matches pattern will return value that matches.
;;; returns false if no value matches pattern
(define find-match
  (lambda (pattern value)
    (cond
      [(match? pattern value)
       value]
      [  (not(list? value)) 
         #f]
      [ (not (pair? value))
        (match? pattern value)]  
      [else
       (or (find-match pattern (car value))
           (find-match pattern (cdr value)))])))

(check-equal? (find-match '(list 1 x)
                          '(((1 2) 3) (2 3) (2 1) (1 4) 5))
              '(1 2)
              "another list case")
(check-equal? (find-match '(cons 2 x)
                          '(((1 2) 3) (2 3) (2 1) (1 4) 5))
              '(2)
              "base cons case")
(check-equal? (find-match '(list 1 x)
                          '((1 2 3) (2 3) (2 1) (1 4) 5))
              '(1 4) "base list case")
(check-equal?  (find-match '(cons 2 (cons x y))
                           '(((1 2) 3) (8 2 4 5) (2 3) (2 1) (1 4) 5))
               '(2 4 5) "edgier cons case")

(check-equal? (find-match '(cons 2 (cons x y))
                          '(((1 2) 3) (2 3) (2 1) (1 4) 5))
              '(2 3) "another cons case")

;;;;; PARRT 3

;;; (binding-helper pattern value)->list?
;;; pattern: any pattern that is in a list
;;; value: value? in list form
;;; returns the nested binding, values for cons list pattern
;;; In general procedure will return'((pattern1 value1)...( pattern valuen)) for length of list.
(define binding-helper
  (lambda (pattern value)
    (if (null? pattern)
        '()
        (cons (list (car pattern) (car value)) (binding-helper (cdr pattern) (cdr value))))))

;;; (bindings pattern value)->list?
;;; pattern: a pattern
;;; value: any value?
;;; returns list of matching bindings and patterns 
(define bindings
  (lambda (pattern value)
    (cond 
      ; Whoo!  We found a binding
      [(symbol? pattern)
       (list (list pattern value))]
      ; Base case of integers or strings.  No bindings.
      [(or (integer? pattern) (string? pattern))
       (list (list pattern value))]
      ;empty list pattern
      [(empty-list-pattern  pattern value)
       (list (list pattern value))]
      ; The rest remains to be written
      [(match-cons? pattern value)
       (if (= (length (cdr pattern)) 2)
           ;;; if pattern is a (cons x y) do this 
           (list (list (car(cdr pattern))(car value)) (list (list-ref pattern 2)(cdr  value)))
           ;;; (if pattern is something like '(cons x y z ) do this..
           (binding-helper (cdr pattern) value))]
      [(list-match? pattern value)
       (binding-helper (cdr pattern) value)]
      [else
       #f ])))

;;;; binding tests
(check-equal? (bindings '(cons x y) (list 1 2 3))
              '((x 1) (y (2 3))) "base cons case")
(check-equal? (bindings '(list) '())
              '(((list) ())) "base case empty list bindings")
(check-equal? (bindings '(cons x y z) (list "a" "b" "c"))
              '((x "a") (y "b") (z "c")) "second cons base case")
(check-equal? (bindings '(cons x y) (list "value"))
              '((x "value") (y ())) "a edge cons case")
(check-equal? (bindings '(list x y z) (list "a" "b" "c"))
              '((x "a") (y "b") (z "c")) "base list  case")
(check-equal? (bindings '(list variable) (list "value"))
              '((variable "value")) "base case simple list")
(check-equal? (bindings 'x 1)
              '((x 1)) "base simple bindings case")
(check-equal? (bindings 'variable "value")
              '((variable "value")) "another base bindings case")


