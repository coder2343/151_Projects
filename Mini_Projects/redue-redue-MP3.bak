#lang racket


;; CSC 151.02 (Fall 2020, Term 2)
;; PROJECT : MiniProject3
;; https://rebelsky.cs.grinnell.edu/Courses/CSC151/2020Fa2/assignments/project03.html
;; Author: Liam Walsh
;; Date: 11-15-20
;; Acknowledgements: CS tutor Vu, Mai Phuong helped me build procedures around   counting sentances and removing puncuation form words.
;;  Sam R helped me get unstuck when I had trouble with first part that delt with counting how offten words apear within a text.

(require csc151)
(require rackunit)
(require racket/undefined)

;wizard of oz text
(define wizard-words(file->words "pg43936.txt"))

(define a (list "Dorothy" "a" "bat" "ball" "yellow" "of"))

(define boynton-01
  "A cow says moo.  A sheep says baa.  Three singing pigs say la la la.  No no, you say, that isn't right.  The pigs say oink all day and night.
Rhinoceroses snort and snuff and little dogs go ruff ruff ruff.  Some other dogs go bow wow.  And cats and kittens say meow.  Quack says the duck.  A horse says neigh.  It's quiet now.  What do you say?")

(define sample "The ball is red. The ball is big. I sing. I jump. I run.")
(define sample-complex "The ball is red. The ball is big. I sing. I jump, with my mom. Dogs, cats, and ducks are my favs. I jump. I run.")

(define samole-pos-expresion "Love a+ sun rainbows golden joy excited")
                                     
(define samole-neg-expresion "depresion sad clouds dark  depresing ")


;;; easy word list
;;; comes from http://countwordsworth.com/download/DaleChallEasyWordList.txt
(define easy-words (file->lines "easy_words.txt"))

;;; pos words list comes from
;;; Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
;;; Proceedings of the ACM SIGKDD International Conference on Knowledge 
;;;; Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, Washington, USA, 
(define pos-words  (file->lines "pos_words.txt"))


;;; neg words list comes from
;;; Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer: Analyzing 
;;; and Comparing Opinions on the Web." Proceedings of the 14th 
;;; International World Wide Web conference (WWW-2005), May 10-14, 2005,Chiba,Japan.
(define neg-words (file->lines "neg_words.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; part 1 how offten does a list of words occur within a file?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; note still useing file to words here because we want all puncutation removed for part 1.


;;; (convert-file-to-words file)->string?
;;; file- file?
;;; returns words contained in file. Note all punctuation marks are removed.
(define convert-file-to-words
  (lambda (file)
    (file->words file)))

;;; (quantity-word word file)->list?
;;; word->string?
;;; returns how often a word occurs in a file.
(define quantity-word
  (lambda (word file)
    (list  word (tally (section string-ci=? <> word)file))))                                

;;; (quantity-of-words words file)->list?
;;; words->list?
;;; file- file?
;;; returns list of how often a list of words occurs within a file.                 
(define quantity-of-words
  (lambda (words file)                           
    (map (section quantity-word <> (convert-file-to-words file))words)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; part 2 readability formula
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (compute-dale-chale-score num-difficult-words total-words num-sentences)->number?
;;; num-difficult-words: positive? Integer?, number of words that don't appear on easy words list
;;; total-words: integer?, total number of words in text
;;; num-sentences: integer?, total number of sentences in text
;;; returns dale chale score with provided inputs
(define compute-dale-chall-score
  (lambda (num-difficult-words total-words num-sentences)
    (let ([word-sentance-ratio (* 0.0496 (/  total-words num-sentences))]
          [difficult-word-ratio ( * 0.1579 (* (/ num-difficult-words total-words) 100))]
          [scale-adjustment-dificult-text  3.6365])
      
      (if ( < 5 (* (/ num-difficult-words total-words) 100))
          (+   scale-adjustment-dificult-text  difficult-word-ratio  word-sentance-ratio )
          (+ difficult-word-ratio word-sentance-ratio)))))

;;; (total-appearances-of-word word list)->number?
;;; word: string?
;;; list: list?
;;; returns the number of occurrences of the string word in a lis
(define total-appearances-of-word
  (lambda (word list) 
    (tally (section string-ci=? <> word) list)))

;;;(total-quanity-of-words words list)->number?
;;; words-string?
;;; list-list?
;;; returns how offten word apears in list.
(define total-quanity-of-words
  (lambda (words list)                          
    (map (section total-appearances-of-word <> list) words)))

;;; (contain-period? str)->boolean?
;;; str: string?
;;; returns #t if inputted word contains period
(define contain-period?
  (lambda (str)
    (equal? (string-ref str (-(string-length str)1))#\.)))

;;; (empty-string? str)->boolean?
;;; str: string?
;;; returns #t if inputted string is empty 
(define empty-string?
  (lambda(str)
    (not (equal? 0 (string-length str)))))

;;; (sentence-count string)->integer?
;;; string: string?
;;; returns the number of sentences within the string
(define sentence-count
  (lambda (string)
    (tally contain-period? (filter empty-string? (string-split  string " " )))))

;;; (remove-punctuation str)->string?
;;; str: string?
;;; returns word with any punctuation marks removed.
(define remove-punctuation
  (lambda (str)
    (let ([last-char (string-ref str (- (string-length str) 1))]
          [first-char (string-ref str 0)]
          [word-minus-end-punctuation (substring str 0 (- (string-length str) 1))]
          [word-minus-beggining-punctuation (substring str 1 (string-length str))]) 
      (cond [( equal? last-char #\.)
             word-minus-end-punctuation]
            [( equal? last-char #\-)
             word-minus-end-punctuation]
            [(equal? last-char #\,)
             word-minus-end-punctuation]
            [(equal? last-char #\?)
             word-minus-end-punctuation]
            [(equal? last-char  #\!)
             word-minus-end-punctuation]
            [(equal? last-char #\:)
             word-minus-end-punctuation]
            [(equal? last-char #\;)
             word-minus-end-punctuation]
            [(equal? last-char #\})
             word-minus-end-punctuation]
            [(equal? last-char #\))
             word-minus-end-punctuation]
            [(equal? last-char #\])
             word-minus-end-punctuation]
            [(equal? last-char #\])
             word-minus-end-punctuation]
            [(equal? first-char #\()
             word-minus-beggining-punctuation]
            [(equal? first-char #\")
             word-minus-beggining-punctuation]
            [(equal? first-char #\))
             word-minus-end-punctuation]
            [(equal? first-char #\[)
             word-minus-beggining-punctuation]
            [(equal? first-char #\{)
             word-minus-beggining-punctuation]
            [else
             (substring str 0 (string-length str))]))))

;;; tests for (remove-puncuation)
(check-equal? (remove-punctuation "[hi")
              "hi" "check begining case")
(check-equal? (remove-punctuation "(hi")
              "hi" "check base begining case")
(check-equal? (remove-punctuation "{hi")
              "hi" "check base begining case")
(check-equal? (remove-punctuation "hi)")
              "hi" "check base end case")
(check-equal? (remove-punctuation "hi?")
              "hi" "check base end case")
(check-equal? (remove-punctuation "hi!")
              "hi" "check base end case")
(check-equal? (remove-punctuation "hi}")
              "hi" "check base end case")
(check-equal? (remove-punctuation "hi.")
              "hi" "check base end case")
(check-equal? (remove-punctuation "hi,")
              "hi" "check base end case")
(check-equal? (remove-punctuation "hi;")
              "hi" "check base end case")
(check-equal? (remove-punctuation "hi:")
              "hi" "check base end case")
(check-equal? (remove-punctuation "hi-")
              "hi" "check base end case")

;;; (str->list-remove-punctuation str)->list?
;;; str: string?
;;; returns list of words without periods and other punction marks.
(define str->list-remove-punctuation
  (lambda (str)
    (map remove-punctuation (filter empty-string? (string-split str" " )))))

;;; (quanity-easy-words str)->integer?
;;; str:string?
;;; returns words in string mathch with easy words list.
(define quantity-easy-words 
  (lambda (str)
    (apply + (total-quanity-of-words easy-words str))))

;;; (number-words string)->integer?
;;; string:string?
;;; returns number of words within string.
(define number-words
  (lambda (string)
    (length (str->list-remove-punctuation string ))))

;;;  (quantity-difficult-words str)->positive? integer?
;;;  str:string?
;;;  returns number of difficult words in a text.
(define quantity-difficult-words
  (lambda (str)
    (let ([all-easy-words (= (number-words str) (quantity-easy-words (str->list-remove-punctuation str )))])
      (if (not all-easy-words)
          (- (number-words str) (quantity-easy-words (str->list-remove-punctuation str )))
          0))))

;;; (dale-chall-score str)->number?
;;; str:string?
;;; returns dale-chall-score of a prescribed string
(define dale-chall-score
  (lambda (str) 
    (compute-dale-chall-score (quantity-difficult-words str) (number-words str) (sentence-count str))))

;;; (score->grade score)->string?
;;; score: positive number?
;;; returns the grade range for a given dale-chall-score.
(define score->grade
  (lambda (score)
    (cond
      [ (<= score 4.9)
        "4th grade or lower"]
      [ ( <=  5.0 score 5.9)
        "5th-6th grade"]
      [  (<= 6.0  score 6.9)
         "7th-8th grade"]
      [ (<= 7.0 score 7.9)
        "9th-10th grade"]
      [ ( <= 8.0  score 8.9)
        "11th-12th grade"]
      [(>= score 9.0)
       "13th-15th grade"]
      [else
       #f])))

;; check equals tests for score->grade
(check-equal? (score->grade 4.9)
              "4th grade or lower" "base case")
(check-equal? (score->grade 5.9)
              "5th-6th grade"  "base case")
(check-equal? (score->grade 7.0)
              "9th-10th grade" "base case")
(check-equal? (score->grade 8.0)
              "11th-12th grade" "base case")
(check-equal? (score->grade 9.0)
              "13th-15th grade" "base case")
(check-equal? (score->grade 0)
              "4th grade or lower" "edge case")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; part 3;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (contain-first-char str char-ref)->boolean?
;;; char-ref-character?
;;; str:string?
;;; returns true if prescribed word matches an input character
(define contain-first-char
  (lambda (str char-ref)
    (char-ci=? (string-ref str 0) char-ref)))

;;; (easy-words-with-first-letter char)->list?
;;; char:character? 
;;; returns easy word list words that begin with the same prescribed letter.
(define list-words-with-first-letter
  (lambda (char file)
    (filter (section contain-first-char <> char)(file->lines file))))

;;; (easy-words-with-first-letter char)->list?
;;; char:character? 
;;; returns easy word list words that begin with the same prescribed letter.
(define easy-words-with-first-letter
  (lambda (char)
    (filter (section contain-first-char <> char)easy-words)))

;;; returns nested list of easy words in alphabetical order.
(define easy-words-set
  (list (easy-words-with-first-letter #\a)
        (easy-words-with-first-letter #\b)
        (easy-words-with-first-letter #\c)
        (easy-words-with-first-letter #\d)
        (easy-words-with-first-letter #\e)
        (easy-words-with-first-letter #\f)
        (easy-words-with-first-letter #\g)
        (easy-words-with-first-letter #\h)
        (easy-words-with-first-letter #\i)
        (easy-words-with-first-letter #\j)
        (easy-words-with-first-letter #\k)
        (easy-words-with-first-letter #\l)
        (easy-words-with-first-letter #\m)
        (easy-words-with-first-letter #\n)
        (easy-words-with-first-letter #\o)
        (easy-words-with-first-letter #\p)
        (easy-words-with-first-letter #\q)
        (easy-words-with-first-letter #\r)
        (easy-words-with-first-letter #\s)
        (easy-words-with-first-letter #\t)
        (easy-words-with-first-letter #\u)
        (easy-words-with-first-letter #\v)
        (easy-words-with-first-letter #\w)
        (easy-words-with-first-letter #\x)
        (easy-words-with-first-letter #\y)
        (easy-words-with-first-letter #\z)))

;;; (easy-word word)->integer?
;;; word:string?
;;; returns 1 if word given is within the easy word list.
(define word-in-list?
  (lambda (word list)
    (let ([first (char-downcase (string-ref word 0))])
      (cond
        [(char=? first #\a)
         (tally  (section string-ci=? <> word) (list-ref list 0))]
        [(char=? first #\b)
         (tally  (section string-ci=? <> word) (list-ref list 1))]
        [(char=? first #\c)
         (tally  (section string-ci=? <> word) (list-ref list 2))]
        [(char=? first #\d)
         (tally  (section string-ci=? <> word) (list-ref list 3))]
        [(char=? first #\e)
         (tally  (section string-ci=? <> word) (list-ref list 4))]
        [(char=? first #\f)
         (tally  (section string-ci=? <> word) (list-ref list 5))]
        [(char=? first #\g)
         (tally  (section string-ci=? <> word) (list-ref list 6))]
        [(char=? first #\h)
         (tally  (section string-ci=? <> word) (list-ref list 7))]
        [(char=? first #\i)
         (tally  (section string-ci=? <> word) (list-ref list 8))]
        [(char=? first #\j)
         (tally  (section string-ci=? <> word) (list-ref list 9))]
        [(char=? first #\k)
         (tally  (section string-ci=? <> word) (list-ref list 10))]
        [(char=? first #\l)
         (tally  (section string-ci=? <> word) (list-ref list 11))]
        [(char=? first #\m)
         (tally  (section string-ci=? <> word) (list-ref list 12))]
        [(char=? first #\n)
         (tally  (section string-ci=? <> word) (list-ref list 13))]
        [(char=? first #\o)
         (tally  (section string-ci=? <> word) (list-ref list 14))]
        [(char=? first #\p)
         (tally  (section string-ci=? <> word) (list-ref list 15))]
        [(char=? first #\q)
         (tally  (section string-ci=? <> word) (list-ref list 16))]
        [(char=? first #\r)
         (tally  (section string-ci=? <> word) (list-ref list 17))]
        [(char=? first #\s)
         (tally  (section string-ci=? <> word) (list-ref list 18))]
        [(char=? first #\t)
         (tally  (section string-ci=? <> word) (list-ref list 19))]
        [(char=? first #\u)
         (tally  (section string-ci=? <> word) (list-ref list 20))]
        [(char=? first #\v)
         (tally  (section string-ci=? <> word) (list-ref list 21))]
        [(char=? first #\w)
         (tally  (section string-ci=? <> word) (list-ref list 22))]
        [(char=? first #\x)
         (tally  (section string-ci=? <> word) (list-ref list 23))]
        [(char=? first #\y)
         (tally  (section string-ci=? <> word) (list-ref list 24))]
        [(char=? first #\z)
         (tally  (section string-ci=? <> word) (list-ref list 25))]
        [else
         #f]))))

;;; (quantity-difficult-words-2 str)->number?
;;; str:string?,of words.
;;; returns quantity of difficult words using updated efficient easy word method.
(define quantity-difficult-words-2
  (lambda (str)
    (let ([toal-easy-words (apply +(map (section word-in-list? <> easy-words-set)
                                        (str->list-remove-punctuation str)))]
          [total-words (number-words str)])
      (if ( not (= toal-easy-words total-words))  
          (- total-words toal-easy-words)  
          0))))

;;; (dale-chall-score2 str)->number?
;;; str:string?
;;; returns dale-chall-score for string using the updated efficient easy words method.
(define dale-chall-score2
  (lambda (str)
    (compute-dale-chall-score (quantity-difficult-words-2 str) (number-words str) (sentence-count str))))

#|
dale-chall-score2 is nuch faster than dale-chall-score

> (define sentence 
    "Twas brillig and the slithy toves did gyre and gimble in the wabe. ")
--------------------------------------------test 1
> (time(dale-chall-score2 (apply string-append (make-list 500 sentence))))
cpu time: 735 real time: 761 gc time: 155
12.783607692307694

> (time(dale-chall-score (apply string-append (make-list 500 sentence))))
cpu time: 23062 real time: 23528 gc time: 6770
12.783607692307694
---------------------------------------------------test 2
> (time(dale-chall-score2 (apply string-append (make-list 1000 sentence))))
cpu time: 1438 real time: 1437 gc time: 283
12.783607692307694

>  (time(dale-chall-score (apply string-append (make-list 1000 sentence))))
cpu time: 73547 real time: 76205 gc time: 22456
12.783607692307694
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;part 4 pos negative words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (pos-words-with-first-letter char)->list?
;;; char-character?
;;; returns list of pos words with prescribed first letter
(define pos-words-with-first-letter
  (lambda (char)
    (filter (section contain-first-char <> char) pos-words)))

;;; pos words list sublisted by alphabetical order
(define pos-words-list
  (list (pos-words-with-first-letter #\a)
        (pos-words-with-first-letter #\b)
        (pos-words-with-first-letter #\c)
        (pos-words-with-first-letter #\d)
        (pos-words-with-first-letter #\e)
        (pos-words-with-first-letter #\f)
        (pos-words-with-first-letter #\g)
        (pos-words-with-first-letter #\h)
        (pos-words-with-first-letter #\i)
        (pos-words-with-first-letter #\j)
        (pos-words-with-first-letter #\k)
        (pos-words-with-first-letter #\l)
        (pos-words-with-first-letter #\m)
        (pos-words-with-first-letter #\n)
        (pos-words-with-first-letter #\o)
        (pos-words-with-first-letter #\p)
        (pos-words-with-first-letter #\q)
        (pos-words-with-first-letter #\r)
        (pos-words-with-first-letter #\s)
        (pos-words-with-first-letter #\t)
        (pos-words-with-first-letter #\u)
        (pos-words-with-first-letter #\v)
        (pos-words-with-first-letter #\w)
        (pos-words-with-first-letter #\x)
        (pos-words-with-first-letter #\y)
        (pos-words-with-first-letter #\z)))

;;; (number-of-pos-words str)->number?
;;; str: string?
;;; returns number of times words in string occurs on pos words list
(define number-of-pos-words
  (lambda (str)
    (apply + (map (section word-in-list? <> pos-words-list) (str->list-remove-punctuation str)))))

;;; (neg-words-with-first-letter char)->list?
;;; char- character?
;;; returns list of neg words that have prescribed first letter
(define neg-words-with-first-letter
  (lambda (char)
    (filter (section contain-first-char <> char) neg-words )))
  
;;; neg words set sublisted by alphabetical order.
(define neg-words-set
  (list  (neg-words-with-first-letter #\a)
         (neg-words-with-first-letter #\b)
         (neg-words-with-first-letter #\c)
         (neg-words-with-first-letter #\d)
         (neg-words-with-first-letter #\e)
         (neg-words-with-first-letter #\f)
         (neg-words-with-first-letter #\g)
         (neg-words-with-first-letter #\h)
         (neg-words-with-first-letter #\i)
         (neg-words-with-first-letter #\j)
         (neg-words-with-first-letter #\k)
         (neg-words-with-first-letter #\l)
         (neg-words-with-first-letter #\m)
         (neg-words-with-first-letter #\n)
         (neg-words-with-first-letter #\o)
         (neg-words-with-first-letter #\p)
         (neg-words-with-first-letter #\q)
         (neg-words-with-first-letter #\r)
         (neg-words-with-first-letter #\s)
         (neg-words-with-first-letter #\t)
         (neg-words-with-first-letter #\u)
         (neg-words-with-first-letter #\v)
         (neg-words-with-first-letter #\w)
         (neg-words-with-first-letter #\x)
         (neg-words-with-first-letter #\y)
         (neg-words-with-first-letter #\z)))

;;; (number-of-neg-words str)->number?
;;; str: string?
;;; returns number of times words in string occurs on neg words list
(define number-of-neg-words
  (lambda (str)
    (apply + (map (section word-in-list? <> neg-words-set) (str->list-remove-punctuation str)))))

;;; (pos-neg str)->string
;;; str-> string? of words
;;; returns negative if # neg words >  # pos words returns negative
;;; if # neg words < # pos words returns positive.
;;; and returns neutral if # neg words = # pos words.
(define posneg
  (lambda (str)
    (let ([numer-neg (number-of-neg-words str)]
          [number-pos (number-of-pos-words str)])
      (cond
        [(> numer-neg number-pos)
         "negative"]
        [(< numer-neg number-pos)
         "positive"]
        [(= numer-neg number-pos)
         "neutral"]
        [else
         #f]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; part 5 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; determine reading speed;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;; information on average W.P.M. and reading speed came from
;;;;;; https://ezinearticles.com/?What-is-the-Average-Reading-Speed-and-the-Best-Rate-of-Reading?&id=2298503

;;; (calculate-average-WMP)->number?
;;; str: string?
;;; returns average reading speed in minutes
;;; based on average reading speed being 225 WMP.
(define calculate-average-reading-speed
  (lambda (str)
    (/  (number-words str) 225)))

;;; (calculate-reading-speed str expertise)->string?
;;; str: string?
;;; expertise string?
;;; returns average reading speed in minutes based off
;;; persons familiarity with the language at hand.
(define calculate-reading-speed
  (lambda (str expertise)
    (cond
      [ (= expertise "novice")
        (/  (number-words str) 150)]
      [(= expertise "adult")
       (/  (number-words str) 250)]
      [(= expertise "expert")
       (/  (number-words str) 500)]
      [else
       "Chose an expertise level novice, adult, or expert"])))