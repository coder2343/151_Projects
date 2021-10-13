#lang racket

(require csc151)
;; CSC 151.02 (Fall 2020, Term 2)
;; PROJECT 1: MiniProject2_revised
;; Author: Liam Walsh
;; Date: November 21st 2020
;; Acknowledgements: cs tutor Vu, Mai Phuong  helped me create procedure to remove spaces.
;;  


;;;  (convert-numbers str)->string? 
;;;  str,string? input that is to be converted into list of numbers
;;;  convert string to list than character 
;;;  note this method was borrowed from my list lab
(define convert-numbers
  (lambda(x)
    ( map char->integer (string->list x))))


(define index-conversion-factor  (char->integer #\0))

;;; (convert-index-to-normalnumbers n)->integer?
;;; returns index converted to normal number
;;; note this method was borrowed from my list lab.
(define convert-index-to-normalnumbers
  (lambda (n)
    (- n index-conversion-factor)))

;;; (string->digits str)->list?
;;; str, string?
;;; note this method was borrowed from my list lab
(define string->digits
  (lambda (str)
    (map convert-index-to-normalnumbers (convert-numbers str))))


;;; returns list of constants that each digit in isbn gets multiped by to verify check digit
(define ISBN-multiplcation (list 10 9 8 7 6 5 4 3 2 ))

;;; (check-digit-math ISBN)->integer?
;;; str: string input that is to be converted into list of numbers       
;;; performs operation to test check digit of ISBN, should return 10 if there is a legitimate ISBN.
(define check-digit-math
  (lambda (isbn)  
    (-  11 ( modulo (apply + (map * ISBN-multiplcation (take isbn 9))) 11))))


;;; (dose-checkdigit-match? str)->boolean?
;;;  str,string?              
;;;  method checks if the check digit is 10 if the ISBN contains an X or it will compare if check-digit-math returns the correct check digit.
(define dose-checkdigit-match?
  (lambda (str)
    (let ([x (- ( char->integer(list-ref (string->list  str) 9))78)]
          [check-digit-output (check-digit-math (string->digits str))]
          [acutal-check-digit (convert-index-to-normalnumbers( char->integer(list-ref(string->list  str)9 )))])
      (or
       (= check-digit-output x)
       (= check-digit-output acutal-check-digit)))))


;;; (ISBN-check str)->number?
;;; str: string input that is to be converted into list of numbers                                
;;; the actual ISBN check method which confirms the string is length 10 and the check digit equals last digit of ISBN. If both these conditions is true than function return true.
(define isbn-check
  (lambda (str)
    (and (equal? (string-length str) 10) (eq? (dose-checkdigit-match? str) #t))))

;;; (convert-pairs lst)-> string?
;;; lst-list?
;;; method returns a specfic number of list
(define convert-pairs
  (lambda (str a)
    (list-ref (string->list str) a)))

;;; (remove-spaces str)->string? 
;;; str, string?       
;;; remove spaces method that will remove any spaces between the input.
(define remove-spaces
  (lambda (str) 
    (apply string-append  (string-split str " "))))


;;; (luhn-algorithm-pairs-operations str a b) -> number?
;;; str, string input that is to be converted into list of numbers
;;; a, integer? 
;;; b, integer? 
;;; this method does the first steps of luhan pair operations by extracting the two digits doubling the second digit
;;; than adding the first and second digits together
(define luhn-algorithum-pairs-operations
  (lambda (str  a b)
    (let ([a (convert-index-to-normalnumbers (char->integer(convert-pairs str a)))]
          [b (convert-index-to-normalnumbers(char->integer(convert-pairs str b)))])
      (+ a
         ( if (< (* 2 b ) 9)
              (- (* 2  b) 9) 
              (* 2 b ))))))

;;; (first-digit-math str a)->integer?
;;; str-string?
;;; a-integer?, index of number wanted to be retrieved.
;;; returns the first digit, doubles it, and sums the digits.
(define first-digit-math
  (lambda (str a) 
    (let ([a (convert-index-to-normalnumbers (char->integer(convert-pairs str a)))])    
      (if (< (* 2 a) 9)
          (- (* 2  a) 9) 
          (* 2  a)))))

;;; (luhn-part2 str)-> number?
;;; str-string? Input that is to be converted into list of numbers.
;;; this method carries out part 2 of luhn algorithm by adding up all the pairs and adding the 15th digit to the pairs.
;;; Then use modulo 10 to return the check digit.
(define luhn-part2
  (lambda (str)
    (modulo (* 9
               (+  (first-digit-math str 0)
                   (luhn-algorithum-pairs-operations str 1 2)
                   (luhn-algorithum-pairs-operations str 3 4)
                   (luhn-algorithum-pairs-operations str 5 6)
                   (luhn-algorithum-pairs-operations str 7 8)
                   (luhn-algorithum-pairs-operations str 9 10)
                   (luhn-algorithum-pairs-operations str 11 12)
                   (luhn-algorithum-pairs-operations str 13 14)))
            10)))

;;; (luhn-check? str ) -> string?
;;; str, string? Input that is to be converted into list of number.s.
;;; procedure that executes entire luhn check algorithm. Returns true if check digit math = 16th digit of the credit card number.
(define luhn-16-check?
  (lambda (str)
    (let ([actual-check-digit (convert-index-to-normalnumbers (char->integer (list-ref (string->list (remove-spaces str)) 15))) ]
          [calculated-check-digit (luhn-part2 (remove-spaces str))])
      (= actual-check-digit calculated-check-digit ))))

;;;  ( test-eq?  a b)->string?
;;;  str-string? Input that is to be converted into list of numbers.
;;;  a-boolean? Output of ISBN or luhn check.
;;;  b-boolean? Expected Output of ISBN or luhn check.
;;;  This will test that the Expected  Output of ISBN or luhn check. Matches the actual function Output.
(define test-eq?
  (lambda (a b)
    (when (not (equal? a b))
      (error "Check failed"))))



(define isbn-check-1 (test-eq? (isbn-check "123456789X") #t)) ;test if isbn-check will return true for a real isbn
(define isbn-check-2 (test-eq? (isbn-check "123456X899") #f)) ;test if isbn-check will return faklse isbn
(define isbn-check-3 (test-eq? (isbn-check "1234568X") #f)) ;test if isbn-check will return false for short isbn
(define isbn-check-4(test-eq? (isbn-check "-12345689X") #f)) ;test if isbn-check will return false for negative input 
(define isbn-check-5 ( test-eq? (isbn-check  "1234557894") #t));test if isbn-check will return true for correct isbn.


;;; acknowledgement used https://simplycalc.com/luhn-validate.php to generate correct luhn number.

(define luhncheck-1 (test-eq?(luhn-16-check? "1234567890123452") #t)) ; test if luhn check returns true for legitiment credit card number
(define luhncheck-2 (test-eq?(luhn-16-check? "1234507890123452") #f)) ; test if luhn check returns false when one digit of legitiment credit card number is changed
(define luhncheck-3 (test-eq?(luhn-16-check? "-1234567890123454") #f)) ; test if luhn check returns false for illegitiment credit card number
(define luhncheck-4 (test-eq?(luhn-16-check? "123 4567 8901 23452") #t)) ; test if luhn check returns true for legitiment credit card number with spaces
(define luhncheck-5 (test-eq?(luhn-16-check? "1234 56789 0123 352") #f)) ; test if luhn check returns false  for illegitiment credit card number withj spaces





                                       



                             

                                       

         
       
                    
                    

                   
                
 

                            
                        
                          
                           
                           
                     
                          
                             





                        
                                




                                

                             



                                                


