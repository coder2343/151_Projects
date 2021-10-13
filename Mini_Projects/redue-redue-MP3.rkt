#lang racket

(require csc151)
(require rackunit)
(require racket/undefined)

;;;;;;;;;;;
;; NOTE TO GRADER
;;;; the dale chall score runs fine on my computer I dont know what the issue could be you need my text files
;;;; so I have atached them to this submision in gradescope and if you use them to check my submison than dale chall score should work fine.

#|
here is what my dale chall score outputs.
> (dale-chall-score2 sample-complex)
6.857267032967033
> (dale-chall-score2 sample)
0.13887999999999998
> (dale-chall-score boynton-01)
9.28385347593583
> (dale-chall-score boynton-01)
9.28385347593583
|#

;; CSC 151.02 (Fall 2020, Term 2)
;; PROJECT : MiniProject3
;; https://rebelsky.cs.grinnell.edu/Courses/CSC151/2020Fa2/assignments/project03.html
;; Author: Liam Walsh
;; Date: 11-15-20
;; Acknowledgements: CS tutor Vu, Mai Phuong helped me build procedures around   counting sentances and removing puncuation form words.
;;  Sam R helped me get unstuck when I had trouble with first part that delt with counting how offten words apear within a text.
;;;
;;; walked through my second round of revisons with CS Tutor Mai to ensure I fixed everything

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sample lists / strings for testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;wizard of oz text
(define wizard-words(file->words "pg43936.txt"))

(define a (list "Dorothy" "a" "bat" "ball" "yellow" "of"))
; Stolen from Sandra Boynton's _Moo, Baa, La La La_.  Transcribed by SamR (almost from memory).
(define boynton-01
  "A cow says moo. A sheep says baa. Three singing pigs say la la la.  No no, you say, that isn't right.  The pigs say oink all day and night. Rhinoceroses snort and snuff and little dogs go ruff ruff ruff.  Some other dogs go bow wow.  And cats and kittens say meow.  Quack says the duck.  A horse says neigh.  It's quiet now.  What do you say?")

(define sample "The ball is red. The ball is big. I sing. I jump. I run.")
(define sample-complex "The ball is red. The ball is big. I sing. I jump, with my mom. Dogs, cats, and ducks are my favs. I jump. I run.")

(define sample-pos-expresion "love a+ sun rainbows golden joy excited")                               
(define sample-neg-expresion "depresion sad clouds dark  depresing")

(define sample-nueatral-expression  "joy excited depresion sad clouds dark  depresing")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; diffrent lists of words used in project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; helper list of all the charectors of the alphabet
(define the-alphabet (list  #\a #\b #\c #\d #\e #\f
                            #\g  #\h  #\i
                            #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u
                            #\v #\w #\x #\y #\z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; part 1 how offten does a list of words occur within a file?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; note still useing file to words here because we want all puncutation removed for part 1.

;;; (convert-file-to-words file)->string?
;;; file, file?
;;; returns words contained in file. Note all punctuation marks are removed.
(define convert-file-to-words
  (lambda (file)
    (file->words file)))

;;; (quantity-word word file)->list?
;;; word, string?
;;; returns how often a word occurs in a file.
(define quantity-word
  (lambda (word file)
    (list word (tally (section string-ci=? <> word)file))))                                

;;; (quantity-of-words words file)->list?
;;; words->list?
;;; file, file?
;;; returns list of how often a list of words occurs within a file.                 
(define quantity-of-words
  (lambda (words file)                           
    (map (section quantity-word <> (convert-file-to-words file))words)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; part 2 readability formula
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (compute-dale-chale-score num-difficult-words total-words num-sentences)->number?
;;; num-difficult-words,  positive? Integer?,number of words that don't appear on easy words list
;;; total-words, nonzero? integer?,total number of words in text
;;; num-sentences, nonzero? integer?,total number of sentences in text
;;; returns dale chale score with provided inputs
(define compute-dale-chall-score
  (lambda (num-difficult-words total-words num-sentences)
    (let ([word-sentence-ratio (* 0.0496 (/  total-words num-sentences))]
          [difficult-word-ratio (* 0.1579 (* (/ num-difficult-words total-words) 100))]
          [scale-adjustment-dificult-text 3.6365])
      (if (< 5 (* (/ num-difficult-words total-words) 100))
          (+ scale-adjustment-dificult-text
             difficult-word-ratio
             word-sentence-ratio)
          (+ difficult-word-ratio word-sentence-ratio)))))

;;; (total-quanity-of-words words list)->number?
;;; words, string?
;;; list, list?
;;; returns how offten word apears in list.
(define total-quanity-of-words
  (lambda (words list)
    (let ([total-appearances-of-word (lambda (word list) 
                                       (tally (section string-ci=? <> word) list))])
      (map (section total-appearances-of-word <> list) words))))

;;; (contain-period? str)->boolean?
;;; str, string?
;;; returns #t if inputted word contains period
(define contain-period?
  (lambda (str)
    (equal? (string-ref str (- (string-length str)1))#\.)))

;;; (sentence-count string)->integer?
;;; string, string?
;;; returns the number of sentences within the string
(define sentence-count
  (lambda (string)
    (tally contain-period? (filter non-empty-string? (string-split  string " ")))))

;;; (remove-punctuation str)->string?
;;; str, string?
;;; returns word with any punctuation marks removed.
(define remove-punctuation
  (lambda (str)
    (let* ([end-puncuation (list #\. #\- #\, #\?  #\! #\: #\;
                                 #\: #\; #\) #\} #\) #\])]
           [begining-puncuation (list #\" #\[ #\{ #\( )]
           [last-char (string-ref str
                                  (- (string-length str) 1))]
           [first-char (string-ref str 0)]
           [word-minus-end-punctuation (substring str 0 (- (string-length str) 1))]
           [word-minus-beginning-punctuation (substring str 1 (string-length str))]
           [contain-end-puncuation? (filter (section equal? last-char <>)
                                            end-puncuation)]
           [contain-begining-puncuation? (filter (section equal? first-char <>)
                                                 begining-puncuation)])
      
      (cond [(and (pair? contain-end-puncuation?)
                  (list-ref contain-end-puncuation? 0))
             word-minus-end-punctuation]
            [(and (pair? contain-begining-puncuation?)
                  (list-ref contain-begining-puncuation? 0))
             word-minus-beginning-punctuation]
            [else
             str]))))

;;; (str->list-remove-punctuation str)->list?
;;; str, string?
;;; returns list of words without periods and other punction marks.
(define str->list-remove-punctuation
  (lambda (str)
    (map remove-punctuation
         (filter non-empty-string? (string-split str" ")))))

;;; (quanity-easy-words str)->integer?
;;; str,string?
;;; returns words in string mathch with easy words list.
(define quantity-easy-words 
  (lambda (str)
    (apply + (total-quanity-of-words easy-words str))))

;;; (number-words string)->integer?
;;; string, string?
;;; returns number of words within string.
(define number-words
  (lambda (string)
    (length (str->list-remove-punctuation string))))

;;;  (quantity-difficult-words str)->positive? integer?
;;;  str, string?
;;;  returns number of difficult words in a text.
(define quantity-difficult-words
  (lambda (str)
    (let ([all-easy-words (= (number-words str)
                             (quantity-easy-words (str->list-remove-punctuation str)))])
      (if (not all-easy-words)
          (- (number-words str)
             (quantity-easy-words (str->list-remove-punctuation str)))
          0))))

;;; (dale-chall-score str)->number?
;;; str, string?
;;; returns dale-chall-score of a prescribed string
(define dale-chall-score
  (lambda (str) 
    (compute-dale-chall-score (quantity-difficult-words str)
                              (number-words str) (sentence-count str))))

;;; (score->grade score)-->string?
;;; score, positive? number?
;;; returns the grade range for a given dale-chall-score.
(define score->grade
  (lambda (score)
    (cond
      [ (<= (floor score) 4.9)
        "4th grade or lower"]
      [ ( <= 5.0 (floor score) 5.9)
        "5th-6th grade"]
      [  (<= 6.0 (floor score) 6.9)
         "7th-8th grade"]
      [ (<= 7.0 (floor score) 7.9)
        "9th-10th grade"]
      [ (<= 8.0 (floor score) 8.9)
        "11th-12th grade"]
      [(>= (floor score) 9.0)
       "13th-15th grade"]
      [else
       #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; part 3;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (contain-first-char str char-ref)->boolean?
;;; char-ref, character?
;;; str, string?
;;; returns true if prescribed word matches an input character
(define contain-first-char
  (lambda (str char-ref)
    (char-ci=? (string-ref str 0) char-ref)))

;;; (words-with-first-letter char file)->list?
;;; char, character?
;;; lst, list?
;;; returns word list words that begin with the same prescribed letter.
(define words-with-first-letter
  (lambda (char lst)
    (filter (section contain-first-char <> char) lst)))

;;; returns nested list of easy words in alphabetical order.
(define easy-words-set
  (map (section words-with-first-letter <> easy-words) the-alphabet))

;;; (word-in-list? word list)->integer?
;;; word, string?
;;; list, list? of of list of words sublisted by alphabetical words
;;; returns 1 if word given is within the imputed word list
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
;;; str, string?,of words.
;;; returns quantity of difficult words using updated efficient easy word method.
(define quantity-difficult-words-2
  (lambda (str)
    (let ([toal-easy-words (apply + (map (section word-in-list? <> easy-words-set)
                                         (str->list-remove-punctuation str)))]
          [total-words (number-words str)])
      (if ( not (= toal-easy-words total-words))  
          (- total-words toal-easy-words)  
          0))))

;;; (dale-chall-score2 str)->number?
;;; str, string?
;;; returns dale-chall-score for string using the updated efficient easy words method.
(define dale-chall-score2
  (lambda (str)
    (compute-dale-chall-score (quantity-difficult-words-2 str)
                              (number-words str) (sentence-count str))))

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

;;; returns nested list of pos words in alphabetical order.
(define pos-words-set
  (map (section words-with-first-letter <> pos-words) the-alphabet))

;;; (number-of-pos-words str)->number?
;;; str,string?
;;; returns number of times words in string occurs on pos words list.
(define number-of-pos-words
  (lambda (str)
    (apply + (map (section word-in-list? <> pos-words-set)
                  (str->list-remove-punctuation str)))))
(define neg-words-set
  (map (section words-with-first-letter <> neg-words) the-alphabet))

;;; (number-of-neg-words str)->number?
;;; str, string?
;;; returns number of times words in string occurs on neg words list
(define number-of-neg-words
  (lambda (str)
    (apply + (map (section word-in-list? <> neg-words-set)
                  (str->list-remove-punctuation str)))))

;;; (posneg str)->string?
;;; str, string? of words.
;;; returns negative if # neg words. >  # pos words. returns negative
;;; if # neg words. < # pos words. returns positive.
;;; and returns neutral if # neg words. = # pos words.
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


;;;; SAM R Says I have drawen from a lot of souces and my code is more than sufficent.

;;;;;; source
;;;;;;;  Website about how to predict book reading length
;;;;;;;;; <https://lifehacker.com/calculate-the-reading-time-for-any-book-1835121105>

;;;;;; source
;;;;;; Information on average W.P.M. and reading speed came from
;;;;;; https://ezinearticles.com/?What-is-the-Average-Reading-Speed-and-the-Best-Rate-of-Reading?&id=2298503

;;;; source
;;;;;;;;;; <https://www.readinglength.com/>
;;;;;;;;;;;;; one formula would be (#words/ 250 WPM)/60 to get number of hours needed to complete book.

;;;;;;; source
;;;;;; all information on books word count comes from
;;;;; <http://wordcounters.com/>

;;; (calculate-average-WMP)->number?
;;; str: string?
;;; returns average reading speed in minutes
;;; based on average reading speed being 225 WMP.
(define calculate-average-reading-speed
  (lambda (str)
    (/ (number-words str)225)))

;;; (calculate-reading-speed str expertise)->string?
;;; str, string?
;;; expertise string?
;;; returns average reading speed in minutes based off
;;; persons familiarity with the language at hand.
(define calculate-reading-speed
  (lambda (str expertise)
    (cond
      [(= expertise "novice")
       (/ (number-words str) 150)]
      [(= expertise "adult")
       (/ (number-words str) 250)]
      [(= expertise "expert")
       (/ (number-words str) 500)]
      [else
       "Chose an expertise level novice, adult, or expert"])))

(define atlas-shruged-word-count 327380)
(define it-word-count 373523)
(define mobey-dick-wordcount 149852)

;;; (total-book-reading-length number-words)->string?
;;; number-words, integer? positive? Total number of words in a book.
;;; reading-spped integer? in worder per minute most people read at 250 WPM
;;; returns how long on average it will take a person to finish a book..
(define total-book-reading-length
  (lambda (number-words reading-speed)
    (let* ([number-min (quotient  number-words reading-speed)]
           [number-hours (quotient number-min 60)]
           [reminder (modulo number-min 60)])  
      (string-append (number->string  number-hours)" " "hours" " ""and"
                     " "(number->string reminder)" " "minutes" " "  "to finish the book"))))

;;; (how-long-to-read-book number-words average-daily-reading-duration reading-spped)->string?
;;; Number-words, integer? Number of words contained in book.
;;; average-daily-reading-duration, number? how many hours do you read on average per a day
;;; reading-spped integer? in worder per minute most people read at 250 WPM
;;; returns how many days on average reading a specific book will take.
(define how-long-to-read-book
  (lambda (number-words average-daily-reading-duration reading-spped)
    (let* ([number-min (quotient  number-words reading-spped)]
           [number-hours (quotient number-min 60)]
           [number-days-to-finish  (/ number-hours average-daily-reading-duration)])
      (string-append "you will take" " "
                     (number->string number-days-to-finish)
                     " days on average to finish reading this boook" " if you spend "
                     (number->string average-daily-reading-duration) " hours a day reading your book"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; apendix tests and output experiements;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tests for removing puncuation
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
(check-equal? (remove-punctuation "[hi")
              "hi" "check begining case")
(check-equal? (remove-punctuation "(hi")
              "hi" "check base begining case")
(check-equal? (remove-punctuation "{hi")
              "hi" "check base begining case")
(check-equal? (remove-punctuation "hi")
              "hi" "check base begining case")
(check-equal? (remove-punctuation "hi'!")
              "hi'" "check base begining case")

;; check equals tests for score->grade
(check-equal? (score->grade 4.9)
              "4th grade or lower" "base case")
(check-equal? (score->grade 5.9)
              "5th-6th grade" "base case")
(check-equal? (score->grade 7.0)
              "9th-10th grade" "base case")
(check-equal? (score->grade 8.0)
              "11th-12th grade" "base case")
(check-equal? (score->grade 9.0)
              "13th-15th grade" "base case")
(check-equal? (score->grade 0)
              "4th grade or lower" "edge case")
(check-equal? (score->grade 4.95)
              "4th grade or lower" "edge case")

#|
;;; sample output tests for average-total-reading-length

;;;;atlas shruged by Ayn Rand 
;;;; how many words 327,380 Words
> (average-total-reading-length atlas-shruged-word-count)
"21 hours and 49 minutes to finish the book"

;;;; It by Steven King
;;;;; 373,523 Words
> (average-total-reading-lengthit-word-count )
"24 hours and 25 minutes to finish the book"

;;;;; Mobey Dick
;;;;;;;;;;; 149,852 Words
> (average-total-reading-length mobey-dick-wordcount)
"9 hours and 59 minutes to finish the book"|#

#|
output experments for how long to read
> (how-long-to-read-book it-word-count 1.25)
"you will take 19.2 days on average to finish reading this boook
if you spend 1.25 hours a day reading your book"

> (how-long-to-read-book  mobey-dick-wordcount 1)
"you will take 9 days on average to finish reading this boook
if you spend 1 hours a day reading your book"

> (how-long-to-read-book atlas-shruged-word-count 1.25)
"you will take 16.8 days on average to finish reading this boook
 if you spend 1.25 hours a day reading your book |#