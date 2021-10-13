#lang racket


;; CSC 151.02 (Fall 2020, Term 2)
;; PROJECT : MiniProject3
;; https://rebelsky.cs.grinnell.edu/Courses/CSC151/2020Fa2/assignments/project03.html
;; Author: Liam Walsh
;; Date: 11-15-20
;; Acknowledgements: CS tutor Vu, Mai Phuong helped me build procedures around   counting sentances and removing puncuation form words.
;;  Sam R helped me get unstuck when I had trouble with first part that delt with counting how offten words apear within a text.

(require csc151)

;wizard of oz text
(define wizard-words(file->words "pg43936.txt"))

(define a (list "dorothy" "d" "bat" "ball" "yellow" "oF"))

(define boynton-01
  "A cow says moo.  A sheep says baa.  Three singing pigs say la la la.  No no, you say, that isn't right.  The pigs say oink all day and night.  Rhinoceroses snort and snuff and little dogs go ruff ruff ruff.  Some other dogs go bow wow.  And cats and kittens say meow.  Quack says the duck.  A horse says neigh.  It's quiet now.  What do you say?")

(define sample "The ball is red. The ball is big. I sing. I jump. I run.")
(define sample-complex "The ball is red. The ball is big. I sing. I jump, with my mom. Dogs, cats, and ducks are my favs. I jump. I run.")

(define samole-pos-expresion "Love a+ sun rainbows golden joy excited")
                                     
(define samole-neg-expresion "depresion sad clouds dark  depresing ")
                                     
;part 1 total how offten does a list of words occur within a file?


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
    (list  word (tally (section string-ci=? <> word) file))))                                

;;; (quantity-of-words words file)->list?
;;; words->list?
;;; file- file?
;;; returns list of how often a list of words occurs within a file.                 
(define quantity-of-words
  (lambda (words file)                           
    (map (section quantity-word <> (convert-file-to-words file))  words) ))


  
;part 2 readability formula

;;; (compute-dale-chale-score num-difficult-words total-words num-sentences)->number?
;;; num-difficult-words- positive? Integer?, number of words that don't apear on easy words list
;;; total-words-Integer?, total number of words in text
;;; num-sentences-Integer?, total number of sentences in text
;;; returns dale chale score with provided inputs
(define compute-dale-chale-score
  (lambda (num-difficult-words total-words num-sentences)
    (if ( < 5 (* (/ num-difficult-words total-words) 100))
        (+ 3.6365   (+ ( * 0.1579 (* (/ num-difficult-words total-words) 100)) (* 0.0496 (/ num-sentences total-words))))

        (+ ( * 0.1579 (* (/ num-difficult-words total-words) 100)) (* 0.0496 (/ num-sentences total-words))))))

;;; (quantity-word-total word list)->number?
;;; word-string?
;;; list-list?
;;; returns total number of word in a list
(define quantity-word-total
  (lambda (word list) 
    (tally (section string-ci=? <> word) list)))


;;;(total-quanity-of-words words list)->number?
;;; words-string?
;;; list-list?
;;; returns number of words present in a string
(define total-quanity-of-words
  (lambda (words list)                          
    (map (section quantity-word-total <>  list)  words)))

;;; (contain-period? str)->booleon?
;;; str-string?
;;; returns #t if inputted word contains period
(define contain-period?
  (lambda (str)
    (equal? (string-ref str (- (string-length str) 1)) #\.)))

;;; (empty-string? str)->booleon?
;;; str-string?
;;; returns #t if inputted string is empty 
(define empty-string?
  (lambda(str)
    (not (equal? 0 (string-length str)))))

;;; (sentence-count string)->integer?
;;; string-string?
;;; returns the number of sentences within the string
(define sentence-count
  (lambda (string)
    (tally contain-period? (filter empty-string? (string-split  string " " )))))

;;; (remove-punctuation str)->string?
;;; str-string?
;;; returns word with any punctuation marks removed.

(define remove-punctuation
  (lambda (str)
    (cond  [(equal? (string-ref str (- (string-length str) 1)) #\.)
            (substring str 0 (- (string-length str) 1))]
           [(equal? (string-ref str (- (string-length str) 1)) #\,)
            (substring str 0 (- (string-length str) 1))]
           [(equal? (string-ref str (- (string-length str) 1)) #\?)
            (substring str 0 (- (string-length str) 1))]
           [(equal? (string-ref str (- (string-length str) 1)) #\!)
            (substring str 0 (- (string-length str) 1))]
           [(equal? (string-ref str (- (string-length str) 1)) #\:)
            (substring str 0 (- (string-length str) 1))]
           [(equal? (string-ref str (- (string-length str) 1)) #\;)
            (substring str 0 (- (string-length str) 1))]
           [(substring str 0  (string-length str) )])))

;;; (str->list-remove-punctuation str)->list?
;;; str-string?
;;; returns list of words without  periods and other punction marks.
(define str->list-remove-punctuation
  (lambda (str)
    (map  remove-punctuation (filter empty-string? (string-split  str " " )))))


;;; (quanity-easy-words str)-> integer?
;;; str-string?
;;; returns words in string mathch with easy words list.
(define quantity-easy-words 
  (lambda (str)
    (apply + ( total-quanity-of-words(file->lines "easy_words.txt") str))))

;;; (number-words string)-> integer?
;;; string- string?
;;; returns number of words within string.
(define number-words
  (lambda (string)
    (length (str->list-remove-punctuation  string  ))))

;;;  (quantity-difficult-words str)-> positive? integer?
;;;  str-string?
;;;  returns number of difficult words in a text.
(define quantity-difficult-words
  (lambda (str) 
    (if ( not (= (number-words str) ( quantity-easy-words  (str->list-remove-punctuation  str ))))
        (- (number-words str)( quantity-easy-words  (str->list-remove-punctuation  str )))
        0)))

;;; (dale-chall-score str)-> number?
;;; str- string?
;;; returns dale-chall-score of a prescribed string
(define dale-chall-score
  (lambda (str) 
    (compute-dale-chale-score (quantity-difficult-words str) (number-words str) (sentence-count str))))

;;; (score->grade score)-> string?
;;; score- positive number?
;;; returns the grade range for a given dale-chall-score.
(define score->grade
  (lambda (score)
    (cond
      [ (<= score 4.9)
        "4th grade or lower"]
      [ (and ( >= score 5.0) (<= score 5.9))
        "5th-6th grade"]
      [ (and ( >= score 6.0) (<= score 6.9))
        "7th-8th grade"]
      [ (and ( >= score 7.0) (<= score 7.9))
        "9th-10th grade"]
      [ (and ( >= score 8.0) (<= score 8.9))
        "11th-12th grade"]
      [(>= score 9.0)
       "9th grade or higher"]
      [else
       #f])))

;part 3

;;; (contain-first-char str char-ref)-> booleon?
;;; char-ref-character?
;;; str-string?
;;; returns true if prescribed word matches an input character
(define contain-first-char
  (lambda (str char-ref)
    (char-ci=? (string-ref str 0) char-ref)))

;;; (easy-words-with-first-letter char)->list?
;;; char- character? 
;;; returns easy word list words that begin with the same prescribed letter.
(define easy-words-with-first-letter
  (lambda (char)
    (filter (section contain-first-char <> char) (file->lines "easy.txt"))))

;;; (easy-word word)-> integer?
;;; word- string?
;;; returns 1 if word given is within the easy word list.
(define easy-word?
  (let ([easy-a-words (easy-words-with-first-letter #\a)]
        [easy-b-words(easy-words-with-first-letter #\b)]
        [easy-c-words(easy-words-with-first-letter #\c)]
        [easy-d-words(easy-words-with-first-letter #\d)]
        [easy-e-words(easy-words-with-first-letter #\e)]
        [easy-f-words(easy-words-with-first-letter #\f)]
        [easy-g-words(easy-words-with-first-letter #\g)]
        [easy-h-words(easy-words-with-first-letter #\h)]
        [easy-i-words(easy-words-with-first-letter #\i)]
        [easy-j-words(easy-words-with-first-letter #\j)]
        [easy-k-words(easy-words-with-first-letter #\k)]
        [easy-l-words(easy-words-with-first-letter #\l)]
        [easy-m-words(easy-words-with-first-letter #\m)]
        [easy-n-words(easy-words-with-first-letter #\n)]
        [easy-o-words(easy-words-with-first-letter #\o)]
        [easy-p-words(easy-words-with-first-letter #\p)]
        [easy-q-words(easy-words-with-first-letter #\q)]
        [easy-r-words(easy-words-with-first-letter #\r)]
        [easy-s-words(easy-words-with-first-letter #\s)]
        [easy-t-words(easy-words-with-first-letter #\t)]
        [easy-u-words(easy-words-with-first-letter #\u)]
        [easy-v-words(easy-words-with-first-letter #\v)]
        [easy-w-words(easy-words-with-first-letter #\w)]
        [easy-y-words(easy-words-with-first-letter #\y)])

    (lambda(word)
      (let ([first (char-downcase (string-ref word 0))])
        (cond
          [(char=? first #\a)
           (tally  (section string-ci=? <> word)  easy-a-words)]
          [(char=? first #\b)
           (tally  (section string-ci=? <> word)  easy-b-words)]
          [(char=? first #\c)
           (tally  (section string-ci=? <> word)  easy-c-words)]
          [(char=? first #\d)
           (tally  (section string-ci=? <> word)  easy-d-words)]
          [(char=? first #\e)
           (tally  (section string-ci=? <> word)  easy-e-words)]
          [(char=? first #\f)
           (tally  (section string-ci=? <> word)  easy-f-words)]
          [(char=? first #\g)
           (tally  (section string-ci=? <> word)  easy-g-words)]
          [(char=? first #\h)
           (tally  (section string-ci=? <> word)  easy-h-words)]
          [(char=? first #\i)
           (tally  (section string-ci=? <> word)  easy-i-words)]
          [(char=? first #\j)
           (tally  (section string-ci=? <> word)  easy-j-words)]
          [(char=? first #\k)
           (tally  (section string-ci=? <> word)  easy-k-words)]
          [(char=? first #\l)
           (tally  (section string-ci=? <> word)  easy-l-words)]
          [(char=? first #\m)
           (tally  (section string-ci=? <> word)  easy-m-words)]
          [(char=? first #\n)
           (tally  (section string-ci=? <> word)  easy-n-words)]
          [(char=? first #\o)
           (tally  (section string-ci=? <> word)  easy-o-words)]
          [(char=? first #\p)
           (tally  (section string-ci=? <> word)  easy-p-words)]
          [(char=? first #\q)
           (tally  (section string-ci=? <> word)  easy-q-words)]
          [(char=? first #\r)
           (tally  (section string-ci=? <> word)  easy-r-words)]
          [(char=? first #\s)
           (tally  (section string-ci=? <> word)  easy-s-words)]
          [(char=? first #\t)
           (tally  (section string-ci=? <> word)  easy-t-words)]
          [(char=? first #\u)
           (tally  (section string-ci=? <> word)  easy-u-words)]
          [(char=? first #\v)
           (tally  (section string-ci=? <> word)  easy-v-words)]
          [(char=? first #\w)
           (tally  (section string-ci=? <> word)  easy-w-words)]
          [(char=? first #\y)
           (tally  (section string-ci=? <> word)  easy-y-words)]
          [else
           #f])))))

;;; (quantity-difficult-words-2 str) -> number?
;;; str-> string?, of words 
;;; returns quantity of difficult words using updated efficient easy word method
(define quantity-difficult-words-2
  (lambda (str) 
    (if ( not (= (number-words str) (apply +(map (section easy-word? <> ) (str->list-remove-punctuation str)))))
        (- (number-words str) (apply +(map (section easy-word? <> ) (str->list-remove-punctuation str)))  )
        0)))

;;; (dale-chall-score2 str) -> number?
;;; str- string?
;;; returns dale-chall-score for string using the updated efficient easy words method.
(define dale-chall-score2
  (lambda (str)
    (compute-dale-chale-score (quantity-difficult-words-2 str) (number-words str) (sentence-count str))))


#|

words? dose not seem to make a diffrence


> (define sentence 
    "Twas brillig and the slithy toves did gyre and gimble in the wabe. ")

> (dale-chall-score2 (apply string-append (make-list 500 sentence)))
12.142623076923078
> (dale-chall-score (apply string-append (make-list 500 sentence)))
12.142623076923078
; does make a diffrence when sentance is repeated 1000 times
> (define sentence 
    "Twas brillig and the slithy toves did gyre and gimble in the wabe. ")
> (time (dale-chall-score2 (apply string-append (make-list 1000 sentence))))
cpu time: 3453 real time: 3517 gc time: 1372
12.142623076923078

> (time (dale-chall-score (apply string-append (make-list 1000 sentence))))
cpu time: 59687 real time: 62354 gc time: 23529
12.142623076923078

|#


;part 4 pos negative words

;;; (pos-words-with-first-letter char)-> list?
;;; char-character?
;;; returns list of pos words with prescribed first letter
(define pos-words-with-first-letter
  (lambda (char)
    (filter (section contain-first-char <> char) (file->lines "pos_words.txt"))))



;;; (pos-words? word)-> number?
;;; word-string?
;;; function returns 1 if word given occurs on pos word list.
(define pos-words?
  (let ([pos-a-words (pos-words-with-first-letter #\a)]
        [pos-b-words(pos-words-with-first-letter #\b)]
        [pos-c-words(pos-words-with-first-letter #\c)]
        [pos-d-words(pos-words-with-first-letter #\d)]
        [pos-e-words(pos-words-with-first-letter #\e)]
        [pos-f-words(pos-words-with-first-letter #\f)]
        [pos-g-words(pos-words-with-first-letter #\g)]
        [pos-h-words(pos-words-with-first-letter #\h)]
        [pos-i-words(pos-words-with-first-letter #\i)]
        [pos-j-words(pos-words-with-first-letter #\j)]
        [pos-k-words(pos-words-with-first-letter #\k)]
        [pos-l-words(pos-words-with-first-letter #\l)]
        [pos-m-words(pos-words-with-first-letter #\m)]
        [pos-n-words(pos-words-with-first-letter #\n)]
        [pos-o-words(pos-words-with-first-letter #\o)]
        [pos-p-words(pos-words-with-first-letter #\p)]
        [pos-q-words(pos-words-with-first-letter #\q)]
        [pos-r-words(pos-words-with-first-letter #\r)]
        [pos-s-words(pos-words-with-first-letter #\s)]
        [pos-t-words(pos-words-with-first-letter #\t)]
        [pos-u-words(pos-words-with-first-letter #\u)]
        [pos-v-words(pos-words-with-first-letter #\v)]
        [pos-w-words(pos-words-with-first-letter #\w)]
        [pos-y-words(pos-words-with-first-letter #\y)]
        [pos-z-words(pos-words-with-first-letter #\z)])

    (lambda(word)
      (let ([first (char-downcase (string-ref word 0))])
        (cond
          [(char=? first #\a)
           (tally  (section string-ci=? <> word)  pos-a-words)]
          [(char=? first #\b)
           (tally  (section string-ci=? <> word)  pos-b-words)]
          [(char=? first #\c)
           (tally  (section string-ci=? <> word)  pos-c-words)]
          [(char=? first #\d)
           (tally  (section string-ci=? <> word) pos-d-words)]
          [(char=? first #\e)
           (tally  (section string-ci=? <> word) pos-e-words)]
          [(char=? first #\f)
           (tally  (section string-ci=? <> word)  pos-f-words)]
          [(char=? first #\g)
           (tally  (section string-ci=? <> word)  pos-g-words)]
          [(char=? first #\h)
           (tally  (section string-ci=? <> word) pos-h-words)]
          [(char=? first #\i)
           (tally  (section string-ci=? <> word)  pos-i-words)]
          [(char=? first #\j)
           (tally  (section string-ci=? <> word) pos-j-words)]
          [(char=? first #\k)
           (tally  (section string-ci=? <> word) pos-k-words)]
          [(char=? first #\l)
           (tally  (section string-ci=? <> word)  pos-l-words)]
          [(char=? first #\m)
           (tally  (section string-ci=? <> word)  pos-m-words)]
          [(char=? first #\n)
           (tally  (section string-ci=? <> word)  pos-n-words)]
          [(char=? first #\o)
           (tally  (section string-ci=? <> word)  pos-o-words)]
          [(char=? first #\p)
           (tally  (section string-ci=? <> word)  pos-p-words)]
          [(char=? first #\q)
           (tally  (section string-ci=? <> word)  pos-q-words)]
          [(char=? first #\r)
           (tally  (section string-ci=? <> word)  pos-r-words)]
          [(char=? first #\s)
           (tally  (section string-ci=? <> word)  pos-s-words)]
          [(char=? first #\t)
           (tally  (section string-ci=? <> word)  pos-t-words)]
          [(char=? first #\u)
           (tally  (section string-ci=? <> word)  pos-u-words)]
          [(char=? first #\v)
           (tally  (section string-ci=? <> word)  pos-v-words)]
          [(char=? first #\w)
           (tally  (section string-ci=? <> word)  pos-w-words)]
          [(char=? first #\y)
           (tally  (section string-ci=? <> word)  pos-z-words)]
          [else
           #f])))))

;;; (number-of-pos-words str)-> number?
;;; str: string?
;;; returns number of times words in string occurs on pos words list
(define number-of-pos-words
  (lambda (str)
    (apply + (map (section pos-words? <> ) (str->list-remove-punctuation str)))))

;;; (neg-words-with-first-letter char)-> list?
;;; char- character?
;;; returns list of neg words that have prescribed first letter
(define neg-words-with-first-letter
  (lambda (char)
    (filter (section contain-first-char <> char) (file->lines "neg_words.txt"))))


;;; (neg-words? word)-> number?
;;; word-string?
;;; function returns 1 if word given occurs on neg word list
(define neg-words?
  (let ([neg-a-words (neg-words-with-first-letter #\a)]
        [neg-b-words(neg-words-with-first-letter #\b)]
        [neg-c-words(neg-words-with-first-letter #\c)]
        [neg-d-words(neg-words-with-first-letter #\d)]
        [neg-e-words(neg-words-with-first-letter #\e)]
        [neg-f-words(neg-words-with-first-letter #\f)]
        [neg-g-words(neg-words-with-first-letter #\g)]
        [neg-h-words(neg-words-with-first-letter #\h)]
        [neg-i-words(neg-words-with-first-letter #\i)]
        [neg-j-words(neg-words-with-first-letter #\j)]
        [neg-k-words(neg-words-with-first-letter #\k)]
        [neg-l-words(neg-words-with-first-letter #\l)]
        [neg-m-words(neg-words-with-first-letter #\m)]
        [neg-n-words(neg-words-with-first-letter #\n)]
        [neg-o-words(neg-words-with-first-letter #\o)]
        [neg-p-words(neg-words-with-first-letter #\p)]
        [neg-q-words(neg-words-with-first-letter #\q)]
        [neg-r-words(neg-words-with-first-letter #\r)]
        [neg-s-words(neg-words-with-first-letter #\s)]
        [neg-t-words(neg-words-with-first-letter #\t)]
        [neg-u-words(neg-words-with-first-letter #\u)]
        [neg-v-words(neg-words-with-first-letter #\v)]
        [neg-w-words(neg-words-with-first-letter #\w)]
        [neg-y-words(neg-words-with-first-letter #\y)]
        [neg-z-words(neg-words-with-first-letter #\z)])

    (lambda(word)
      (let ([first (char-downcase (string-ref word 0))])
        (cond
          [(char=? first #\a)
           (tally  (section string-ci=? <> word)  neg-a-words)]
          [(char=? first #\b)
           (tally  (section string-ci=? <> word)  neg-b-words)]
          [(char=? first #\c)
           (tally  (section string-ci=? <> word)  neg-c-words)]
          [(char=? first #\d)
           (tally  (section string-ci=? <> word) neg-d-words)]
          [(char=? first #\e)
           (tally  (section string-ci=? <> word) neg-e-words)]
          [(char=? first #\f)
           (tally  (section string-ci=? <> word)  neg-f-words)]
          [(char=? first #\g)
           (tally  (section string-ci=? <> word)  neg-g-words)]
          [(char=? first #\h)
           (tally  (section string-ci=? <> word) neg-h-words)]
          [(char=? first #\i)
           (tally  (section string-ci=? <> word)  neg-i-words)]
          [(char=? first #\j)
           (tally  (section string-ci=? <> word) neg-j-words)]
          [(char=? first #\k)
           (tally  (section string-ci=? <> word) neg-k-words)]
          [(char=? first #\l)
           (tally  (section string-ci=? <> word)  neg-l-words)]
          [(char=? first #\m)
           (tally  (section string-ci=? <> word)  neg-m-words)]
          [(char=? first #\n)
           (tally  (section string-ci=? <> word)  neg-n-words)]
          [(char=? first #\o)
           (tally  (section string-ci=? <> word)  neg-o-words)]
          [(char=? first #\p)
           (tally  (section string-ci=? <> word)  neg-p-words)]
          [(char=? first #\q)
           (tally  (section string-ci=? <> word)  neg-q-words)]
          [(char=? first #\r)
           (tally  (section string-ci=? <> word)  neg-r-words)]
          [(char=? first #\s)
           (tally  (section string-ci=? <> word)  neg-s-words)]
          [(char=? first #\t)
           (tally  (section string-ci=? <> word)  neg-t-words)]
          [(char=? first #\u)
           (tally  (section string-ci=? <> word)  neg-u-words)]
          [(char=? first #\v)
           (tally  (section string-ci=? <> word) neg-v-words)]
          [(char=? first #\w)
           (tally  (section string-ci=? <> word)  neg-w-words)]
          [(char=? first #\y)
           (tally  (section string-ci=? <> word)  neg-z-words)]
          [else
           #f])))))

;;; (number-of-neg-words str)-> number?
;;; str: string?
;;; returns number of times words in string occurs on neg words list
(define number-of-neg-words
  (lambda (str)
    (apply + (map (section neg-words? <> ) (str->list-remove-punctuation str)))))

;;; (pos-neg str)->string
;;; str-> string of words
;;; returns negative if # neg words >  # pos words returns negative
;;if # neg words < # pos words returns positive.
;;; and returns neutral if # neg words = # pos words.
(define posneg
  (lambda (str)
    (cond
      [( > (number-of-neg-words str) (number-of-pos-words str)) "negative"]
      [( < (number-of-neg-words str) (number-of-pos-words str)) "positive"]
      [( = ( number-of-neg-words str) ( number-of-pos-words str)) "neutral" ]
      [else
       #f])))

;part5 complex expresions

;;; ( contain-punction? str char) -> booleon?
;;; char-> character?
;;; string-> string? , of words
;;; returns #t if inputed word contains prescribed punctuation
(define contain-punction?
  (lambda (str char)
    (equal? (string-ref str (- (string-length str) 1)) char)))


;;; (coma-count string) -> number?
;;; string-> string? , of words
;;;; returns number of commas in a string
(define coma-count
  (lambda (string)
    (tally (section contain-punction?  <> #\, ) (filter empty-string? (string-split  string " " )))))

;;; (colon-count string)->string?
;;; string->string?
;;;; returns number of colons in a string
(define colon-count
  (lambda (string)
    (tally (section contain-punction?  <> #\: ) (filter empty-string? (string-split  string " " )))))

;;; (semi-colon-count string)->string?
;;; string->string?
;;; returns number of semicolons in a string.
(define semi-colon-count
  (lambda (string)
    (tally (section contain-punction?  <> #\; ) (filter empty-string? (string-split  string " " )))))

;;; (question-mark-count string)->string?
;;; strintg->string?
;;; returns number of ? marks in string
(define question-mark-count
  (lambda (string)
    (tally (section contain-punction?  <> #\? ) (filter empty-string? (string-split  string " " )))))

;;;(exclamation-mark-count string)->string?
;;; string- string?
;;; returns number of exclamation marks in a string.
(define exclamation-mark-count
  (lambda (string)
    (tally (section contain-punction?  <> #\! ) (filter empty-string? (string-split  string " " )))))

;;; (writing-verity str) -> number
;;; str-string?, of words 
;;; returns a ratio of non-period punctuation To periods to measure the complexity of writing.
;;; if no periods are present in the string the function will just returns the sum of non period punctuation marks present.
(define writing-verity
  (lambda (str)
    (if (= sentence-count 0)
        ({+}(coma-count str) ( colon-count str) (semi-colon-count str) (exclamation-mark-count str) ( question-mark-count str))   
        (/ ({+}(coma-count str) ( colon-count str) (semi-colon-count str) (exclamation-mark-count str)( question-mark-count str)) (sentence-count str )))))

  































