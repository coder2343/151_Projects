#lang racket

(require csc151)
;; CSC 151.02 (Fall 2020, Term 2)
;; PROJECT 1: MiniProject2_revised
;; Author: Liam Walsh
;; Date: 11-15-20
;; Acknowledgements:
;;  


;;;  (str  )->string 
;;;  str: string input that is to be converted into list of numbers
;;;  convert string to list than charector 
;;;  note this method was barrowed from my list lab
(define convert-numbers
  (lambda(x)
    ( map char->integer (string->list x))
    ))

;note this method was barrowed from my list lab.
;Subtract 48 converts numbers to standard numers
(define subtract-48
  (lambda (n)
    (- n 48)
    ))

;note this method was barrowed from my list lab
(define string->digits(lambda (str)
                        (map subtract-48 (convert-numbers str))
                        ))


;;; (str  )->string 
;;; str: string input that is to be converted into list of numbers       
;;; performs operation to test check didgit of ISBN. should return 10 if their a legitiment isbn
(define check-digit-math (lambda (isbn)
                  
                           (- 11 ( modulo  
                                   (+
                                    (* 10 (list-ref isbn 0)) (* 9 (list-ref isbn 1)) (* 8 (list-ref isbn 2)) (* 7 (list-ref isbn 3)) (* 6 (list-ref isbn 4))
                                    (* 5 (list-ref isbn 5)) (* 4 (list-ref isbn 6)) (* 3 (list-ref isbn 7)) (* 2 (list-ref isbn 8))
                                    )
                                   11))
                   
                           ))



;;;  (str  )->string 
;;;  str: string input that is to be converted into list of numbers                 
;;;  method checks if the check digit is 10 if the isbn contains an x or it will compare if x=check equals the approximenrt check didgit

(define dose-checkdigit-match? (lambda (str)
                                 (or
                                  (=
                                   (  check-digit-math ( string->digits str)) (- ( char->integer(list-ref (string->list  str) 9))78)
                                   )
                                  (=
                                   ( check-digit-math (string->digits str) ) (subtract-48( char->integer(list-ref (string->list  str)9 )) )
                                   )
                                  )
                                 ))

;;; (str  ) ->string 
;;; str: string input that is to be converted into list of numbers                                
;;; the acutal isbn check method which confirms the string is length 10 and the check didgit equals last digit of isbn. if both these condtions is true than they return true.
(define isbn-check (lambda (str)
 
                     (if (and (equal? (string-length  str) 10) ( eq? ( dose-checkdigit-match? str) #t))
                         #t
                       
                         #f
                       
                         ))               
  )

;;; (n  ) ->number
;;; n: an exact number entered into racket
;;; checks of input entered equals 32
(define equal-32
  (lambda (n)
    (= n 32)
    ))
; (lst)-? string a->nimber
; (str) string
; a: numbr
;method used to return a specfic number of list
(define convert-pairs  (lambda (lst a)
                         (list-ref (string->list lst) a)
                         ))

;;; (str  ) ->string 
;;; str: string input that is to be converted into list of numbers        
;;; remove spaces method that will remove any spaces beteween the input
;;; adnowdlgement had help from cs tutor Vu, Mai Phuong in makeing this method
(define remove-spaces (lambda (str) 
                        (if (map equal-32  ( convert-numbers str))
                            (apply string-append  (string-split str " "))
                            #f)
                        ))


;;; (str  a b) -> string and integers
;;;   str: string input that is to be converted into list of numbers
;;;   a  : integer of list index
;;;   b : integer of list index
;;; this method does the first steps of luhan pair operations by extracting the two didges doubleing the second didiget
;;; than adding the first and second didget together

(define luhn-algorithum-pairs-operations(lambda (str  a b)      
                                          (+
                                           (subtract-48
                                            (char->integer(convert-pairs str a)
                                                          )
                                            )
                                           (* 2
                                              (subtract-48
                                               (char->integer(convert-pairs str b) ))
                                              )
                                           )
                                          ))
;;; (str  ) -> string 
;;;   str: string input that is to be converted into list of numbers
;;; this method carries ou part 2 of luhn algorithum by adding up all the pairs and adding the 15th didgit to the pairs. ;Then use mudulo 10 to reuturn the chekc didgit
(define luhn-part2(lambda (str)
                    (modulo
                     (* (+
                         ( subtract-48 (char->integer(list-ref (string->list str) 14)))
                         (luhn-algorithum-pairs-operations str 0 1)
                         (luhn-algorithum-pairs-operations str 2 3)
                         (luhn-algorithum-pairs-operations str 4 5)
                         (luhn-algorithum-pairs-operations str 6 7)
                         (luhn-algorithum-pairs-operations str 8 9)
                         (luhn-algorithum-pairs-operations str 10 11)
                         (luhn-algorithum-pairs-operations str 12 13)
                         )
                        9)
                     10)
                    ))

;;; (str  ) -> string 
;;;   str: string input that is to be converted into list of numbers
;;;procedure that executures entire luhn check algorithum. Returns true if check didgi math = 16th digit of credit card #
(define luhn-check? (lambda (str) 
                      (if (= (subtract-48( char->integer(list-ref (string->list (remove-spaces str) )15))) (luhn-part2 (remove-spaces str)))
                          #t
                          #f)
                      ))

;;; (str  a b) -> string and booleon state
;;;   str: string input that is to be converted into list of numbers
;;;   a  :  booleon output of isbn or luhn check
;;;   b : expected booleon output of isbn or luhn check
;;;  This will test that the expected  output of isbn or luhn check matches the actual function output
(define test-eq?(lambda (a b)
                  (if(eq? a b)
                     "check is sucessful"
                     #f)
                  ))

(define isbncheck-1 (test-eq?(isbn-check "123456789X") #t)) ;test if isbn check will return true for a legitabte isbn
(define isbncheck-2 (test-eq?(isbn-check "-123456789X") #f)) ;test if isbn check will return false for a false isbn
(define isbncheck-3 (test-eq?(isbn-check "1236789X") #f)) ;test if isbn check will return false for a  # that has less then 10 didgets
(define isbncheck-4 (test-eq?(isbn-check "1234893304") #t));test if isbn check will return true for a good isbn  
(define isbncheck-5 (test-eq?(isbn-check "0234893304") #f));test if isbn check will return false for a bad isbn
(define isbncheck-6 (test-eq?(isbn-check "shakira is awsome") #f));test if isbn check will return true for a false

(define luhncheck-1 (test-eq?(luhn-check? "1234567890123454") #t)) ; test if luhn check returns true for legitiment credit card number
(define luhncheck-2 (test-eq?(luhn-check? "-1234567890123454") #f)) ; test if luhn check returns false for illegitiment credit card number
(define luhncheck-3 (test-eq?(luhn-check? "0034567890123454") #f)); test if luhn check returns false for illegitiment credit card number
(define luhncheck-4 (test-eq?(luhn-check? "1234 5289 97503 542") #f)); test if luhn check returns false for illegitiment credit card number
(define luhncheck-5 (test-eq?(luhn-check? "1234 5889 97503 542") #t)); test if luhn check returns true for a legitment  credit card number entered with spaces







                                       



                             

                                       

         
       
                    
                    

                   
                
 

                            
                        
                          
                           
                           
                     
                          
                             





                        
                                




                                

                             



                                                


