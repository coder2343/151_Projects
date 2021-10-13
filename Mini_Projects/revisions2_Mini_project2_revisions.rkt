#lang racket
(require csc151)
;; CSC 151.02 (Fall 2020, Term 2)
;; PROJECT  MiniProject2_revised
;; Author: Liam Walsh
;; Date: November 21st 2020
;; Acknowledgements: cs tutor Vu, Mai Phuong  helped me create procedure to remove spaces.
;;; showed revisons over to CS tutor Timothy to ensure I covered all the things that I needed to correct.

;;;  (convert-numbers str)->list?
;;;  str, string? Input that is to be converted into list of numbers.
;;;  convert string to list than character
;;;
;;;  note this method was borrowed from my list lab.
(define convert-numbers
  (lambda(x)
    (map char->integer (string->list x))))

(define index-conversion-factor (char->integer #\0))

;;; (convert-index-to-normal-numbers n)->integer?
;;; returns index converted to normal number.
;;;
;;; note this method was borrowed from my list lab.
(define convert-index-to-normal-numbers
  (lambda (n)
    (- n index-conversion-factor)))

;;; (string->digits str)->list?
;;; str, string?
;;;
;;; note this method was borrowed from my list lab.
(define string->digits
  (lambda (str)
    (map convert-index-to-normal-numbers (convert-numbers str))))

;;; (convert-digit-to-number str)->number?
;;; str,string?
;;; Turns a inputed string digit into a number.
(define convert-digit-to-number
  (lambda (str)
    (convert-index-to-normal-numbers (char->integer str))))

;;; returns list of constants that each digit in isbn gets multiped by to verify check digit
(define ISBN-multiplcation (list 10 9 8 7 6 5 4 3 2))

;;; (check-digit-math ISBN)->integer?
;;; str, string? input that is to be converted into list of numbers       
;;; returns calculated ISBN check digit. Should equal 10th number of ISBN.
(define check-digit-math
  (lambda (isbn)  
    (- 11 (modulo (apply + (map * ISBN-multiplcation (take isbn 9))) 11))))

;;; (does-check-digit-match? str)->boolean?
;;; str,string?              
;;; returns true if ISBN check digit matches 10th digit of ISBN
;;; note if the check digit is 10 than ISBN will have X rather than 10.
(define does-check-digit-match?
  (lambda (str)
    (let ([x (- (char->integer (list-ref (string->list str) 9))78)]
          [check-digit-output (check-digit-math (string->digits str))]
          [acutal-check-digit (convert-digit-to-number (list-ref (string->list str) 9))])
      (or (= check-digit-output x)
          (= check-digit-output acutal-check-digit)))))

;;; (ISBN-check str)->boolean?
;;; str,string? Input that is to be converted into list of numbers.
;;; returns true if ISBN is length of 10 and calculated check-digit matches 10th digit of ISBN.
(define ISBN-check?
  (lambda (str)
    (and (equal? (string-length str) 10)
         (does-check-digit-match? str))))

;;; (convert-pairs str a)->string?
;;; str, string?
;;; a, integer?
;;; returns a specific part of a string
(define convert-pairs
  (lambda (str a)
    (list-ref (string->list str) a)))

;;; (remove-spaces str)->string?
;;; str,string?       
;;; returns string with any spaces between inputs removeed.
(define remove-spaces
  (lambda (str) 
    (apply string-append (string-split str " "))))

;;; (luhn-algorithm-pairs-operations str a b)->integer?
;;; str,string? Input that is to be converted into list of numbers.
;;; a,integer? 
;;; b,integer? 
;;; this method does the first steps of luhn pair operations by extracting the two digits doubling the second digit
;;; than adding the first and second digits together.
(define luhn-algorithm-pairs-operations
  (lambda (str a b)
    (let* ([a (convert-index-to-normal-numbers
               (char->integer (convert-pairs str a)))]
           [b (convert-index-to-normal-numbers
               (char->integer (convert-pairs str b)))]
           [b-doubled (* 2 b)])
      (+ a
         (if (> b-doubled 9)
             (- b-doubled 9) 
             b-doubled)))))

;;; (first-digit-math str a)->integer?
;;; str, string?
;;; a, integer? Index of number wanted to be retrieved.
;;; returns the first digit,appropriately doubled
;;; if bigger than 9 than the doubled digit is subtracted by 9.
(define first-digit-math
  (lambda (str a) 
    (let* ([a (convert-index-to-normal-numbers
               (char->integer(convert-pairs str a)))]
           [a-doubled (* 2 a)])    
      (if (> a-doubled 9)
          (- a-doubled 9) 
          a-doubled))))

;;; (calculate-check-digit str)->integer?
;;; str,string? input that is to be converted into list of numbers.
;;; returns sum of all pairs plus first digit. the sum is multiplied by 9.
;;; Then modulo 10 of sum is taken to return the check digit.
(define calculate-check-digit
  (lambda (str)
    (modulo (* 9
               (apply + (first-digit-math str 0)
                      (map (section luhn-algorithm-pairs-operations str <> <>)
                           (list 1 3 5 7 9 11 13) (list  2 4 6 8 10 12 14))))
            10)))

;;; (luhn-16-check? str)->boolean?
;;; str, string? 
;;; Returns true if calculated check digit equals the 16th digit of the credit card number.
(define luhn-16-check?
  (lambda (str)
    (let* ([sixteenth-digit (convert-digit-to-number
                             (list-ref (string->list (remove-spaces str)) 15))]
           [calculated-check-digit (calculate-check-digit (remove-spaces str))])
      (= sixteenth-digit calculated-check-digit))))

;;;  (test-eq? a b)->boolean?
;;;  a, procedure? Procedure call to ISBN or luhn-16-check.
;;;  b, boolean? Expected output of ISBN or luhn check.
;;;  This will test that the expected output of ISBN or luhn check matches the procedure call's output.
(define test-eq?
  (lambda (a b)
    (when (not (equal? a b))
      (error "Check failed"))))

(define isbn-check-1 (test-eq? (ISBN-check? "123456789X") #t)) ;test if isbn-check will return true for a real isbn
(define isbn-check-2 (test-eq? (ISBN-check? "123456X899") #f)) ;test if isbn-check will return faklse isbn
(define isbn-check-3 (test-eq? (ISBN-check? "1234568X") #f)) ;test if isbn-check will return false for short isbn
(define isbn-check-4 (test-eq? (ISBN-check? "-12345689X") #f)) ;test if isbn-check will return false for negative input 
(define isbn-check-5 (test-eq? (ISBN-check?  "1234557894") #t));test if isbn-check will return true for correct isbn.

;;; acknowledgement used https://simplycalc.com/luhn-validate.php to generate correct luhn number.
(define luhncheck-1 (test-eq?(luhn-16-check? "1234567890123452") #t)) ; test if luhn check returns true for legitiment credit card number
(define luhncheck-2 (test-eq?(luhn-16-check? "1234507890123452") #f)) ; test if luhn check returns false when one digit of legitiment credit card number is changed
(define luhncheck-3 (test-eq?(luhn-16-check? "-1234567890123454") #f)) ; test if luhn check returns false for illegitiment credit card number
(define luhncheck-4 (test-eq?(luhn-16-check? "123 4567 8901 23452") #t)) ; test if luhn check returns true for legitiment credit card number with spaces
(define luhncheck-5 (test-eq?(luhn-16-check? "1234 56789 0123 352") #f)); test if luhn check returns false  for illegitiment credit card number withj spaces
(define luhncheck-6 (test-eq?(luhn-16-check? "1000 0000 0000 0008") #t)) ;; other check test





                                       



                             

                                       

         
       
                    
                    

                   
                
 

                            
                        
                          
                           
                           
                     
                          
                             





                        
                                




                                

                             



                                                


