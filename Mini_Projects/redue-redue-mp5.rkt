#lang racket
(require csc151)
(require rackunit)
(require racket/undefined)

;; CSC 151.02 (Fall 2020, Term 2)
;; PROJECT : MiniProject 5
;; https://rebelsky.cs.grinnell.edu/Courses/CSC151/2020Fa2/assignments/project05.html
;; Author: Liam Walsh
;; Date:  December -22-2020
;; Acknowledgements:

;;; citation List of one syllable words used in part 1 came from https://sites.google.com/site/graceguts/stories/a-story-in-100-one-syllable-words
;;;  https://sites.google.com/site/graceguts/stories/another-story-in-100-one-syllable-words
;;;, and https://sites.google.com/site/graceguts/stories/spam-yet-another-story-in-100-one-syllable-words

;;;;; citiation
;;;;  List of common English double vowel sounds used for finding English digraphs
;;; https://en.wikipedia.org/wiki/Diphthong#:~:text=Technically%2C%20a%20diphthong%20is%20a,diphthongs%2C%20one%20in%20every%20syllable.

;;; citation
;;; Used for finding concrete vowel counting rules for
;;;; for determining how many syllables a word has.
;;; https://www.howmanysyllables.com/howtocountsyllables

;;;; citation
;;;; source used to get list of english vowel trigraph patterns
;;;; going to be used to make syllable counting more acureute
;;;; <https://www.theschoolrun.com/what-is-a-trigraph>

;;;;;;;;;;;;;;;;;;;; citation
;;;;;;;;;;;;;;;;;;;; sourcee used the to help find near rymes
;;;;;;;;;;;;;;;;;; <https://www.rhymezone.com/>

;;;;;;;;;;;;;;;;;;;; citation
;;;;;;;;;;;;;;;;;;;; adtional sourcee used to better understand rymeing subtypes 
;;;;;;;;;;;;;;;;;;;; <https://literaryterms.net/rhyme/>

;;;;;;;;;;;;;;;; citation
;;;; source  on limeric poem style that was consulted in this project comes from
;;;;;;; <https://poets.org/glossary/limerick>

;;;;;;;;;;; citation
;;;;;;;;;; jane eyre text comes from <http://www.gutenberg.org/files/1260/1260-0.txt>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; helper proecdures used boardly in the lab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; (contain-period? str)->boolean?
;;; str: string?
;;; returns #t if inputted word contains period
;;;
;;; coppied from mini project 3 code
(define word-ends-in-period?
  (lambda (str)
    (equal? (string-ref str (-(string-length str)1))#\.)))

;;; (empty-string? str)->booleon?
;;; str-string?
;;; returns #t if inputted string is empty
;;;
;;; coppied from MINI PROJECT 3
(define not-empty-string?
  (lambda(str)
    (not (equal? 0 (string-length str)))))

;;; (random-elt lst)->any?
;;; lst : listof any?
;;; Randomly select an element of `lst`
;;;
;;; coppied from my randomness lab
(define random-elt
  (lambda (lst)
    (list-ref lst (random (length lst)))))

;;; (last-char word)->charector?
;;; word, string?
;;; returns last charector of a word
(define last-char
  (lambda (word)
    (string-ref word (- (string-length word) 1))))

;;; (second-last-char word)->charector?
;;; word, string?
;;; returns second to last charector of a word
(define second-last-char
  (lambda (word)
    (string-ref word (- (string-length word) 2))))

;;; (third-last-char word)->charector?
;;; word, string?
;;; returns third to last charector of a word
(define third-last-char
  (lambda (word)
    (string-ref word (- (string-length word) 3))))
  
(define jane-eyre (file->string "Jane Eyre.txt"))

(define jane-string (string-replace
                     (string-replace jane-eyre "\r" "") "\n" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; part 1 Haiku
;;;;;;;; note made this before Sam R switched intructions for part. 1 he
;;;;;;;;; said it was ok to not change to new way.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define one-syllable-words (filter not-empty-string?(file->lines "single-sylable-words.txt")))

;;; acknowledgement list of words came from https://en.wiktionary.org/wiki/Category:English_2-syllable_words
(define two-syllable-words (filter not-empty-string?(file->lines "two-syllable-word-list.txt")))

;;; acknowledgement list of words came from https://en.wiktionary.org/w/index.php?title=Category:English_3-syllable_words
(define three-syllable-words (filter not-empty-string?(file->lines "three-sylable-words.txt")))

;;; acknowledgement list of words came from https://en.wiktionary.org/w/index.php?title=Category:English_4-syllable_words
(define four-syllable-words (filter not-empty-string?(file->lines "four-syllable-words-list.txt")))

;;; acknowledgement list of words came from https://en.wiktionary.org/w/index.php?title=Category:English_5-syllable_words
(define five-syllable-words (filter not-empty-string?(file->lines "five-syllable-words.txt")))

;;; (two-syllable-group)->string?
;;; returns string of two syllable words.
(define two-syllable-group
  (lambda ()
    ( let ([ random-determinet (random 2)])
       (cond
         [( = random-determinet 0)
          (string-append (random-elt one-syllable-words) " "
                         (random-elt one-syllable-words))]
         [ (= random-determinet 1)
           (random-elt two-syllable-words)]
         [else
          "therese an error"]))))       
    
;;; (three-syllable-group ())->strings?
;;; returns a three syllable word group in random fashion each time the procedure is called.
(define three-syllable-group
  (lambda ()
    ( let ([ random-determinet (random 4)])
       (cond
         [(= random-determinet 0)
          (string-append (two-syllable-group) " "
                         (random-elt one-syllable-words) " "
                         (random-elt one-syllable-words))]
         [ ( = random-determinet  1)
           (string-append (random-elt one-syllable-words) " "
                          (random-elt one-syllable-words) " "
                          (two-syllable-group))]
         [(= random-determinet 2)
          (string-append (random-elt one-syllable-words) " "
                         (random-elt one-syllable-words) " "
                         (random-elt one-syllable-words))]
         [(= random-determinet 3)
          (string-append (random-elt three-syllable-words))]   
         [else
          (three-syllable-group)]))))

;;; (four-syllable-group)->string?
;;; returns string of words that are
(define four-syllable-group
  (lambda ()
    ( let ([ random-determinet (random 4)])
       (cond
         [( = random-determinet 0)
          (string-append (random-elt one-syllable-words) " "
                         ( three-syllable-group) )]
         [( = random-determinet  1)
          (string-append ( three-syllable-group) " "
                         (random-elt one-syllable-words))]
         [(= random-determinet 2)
          (string-append ( two-syllable-group) " " ( two-syllable-group))  ]
         [(= random-determinet 3)
          (random-elt four-syllable-words)]
         [else
          #f]))))

;;; (five-syllable-group)->string?
;;; returns list of five syllable words
(define five-syllable-group
  (lambda ()
    ( let ([ random-determinet (random 5)]
           [ chose-long-word (random 5)])
       (cond
         [ (and (= chose-long-word 0) ( = random-determinet 0))
           (string-append(random-elt one-syllable-words) " "
                         ( four-syllable-group))]
         [ (and (= chose-long-word 0) ( = random-determinet 1))
           (string-append ( four-syllable-group) " "
                          (random-elt one-syllable-words))]
         [ (and (= chose-long-word 1) ( = random-determinet 2))
           
           (string-append ( two-syllable-group) " "
                          ( three-syllable-group))]
         [ (and (= chose-long-word 1) ( = random-determinet 3))
           (string-append ( three-syllable-group) " "
                          ( two-syllable-group) )]
         [ (and (> chose-long-word 1) ( = random-determinet 4))
           (string-append (random-elt five-syllable-words) )]
         [else 
          (five-syllable-group)]))))

;;; (seven-syllable-group)->string?
;;; returns list of seven syllable words
(define seven-syllable-group
  (lambda ()
    ( let ([ random-determinet (random 4)])
       (cond
         [ (= random-determinet 0)   
           (string-append (five-syllable-group) " " (two-syllable-group))]
         [(= random-determinet 1)
          (string-append (three-syllable-group) " " (four-syllable-group))]
         [(= random-determinet 3)
          (string-append   (four-syllable-group) " " (three-syllable-group))]
         [(= random-determinet 2) 
          (string-append (two-syllable-group) " " (five-syllable-group))]
         [else
          #f]))))

;;; (haiku)->string?
;;; returns a really bad haiku poem that is  biased to short words
(define haiku
  (lambda ()
    (string-append (five-syllable-group) "\n"
                   ( seven-syllable-group) "\n"
                   (five-syllable-group))))

;;; (haiku-2)->string?
;;; returns a really bad haiku poem that is  less biased to short words                      
(define haiku-2
  (lambda ()
    (let ([top  (five-syllable-group)]
          [middle (seven-syllable-group)]
          [bottom (five-syllable-group)]
          [seven-syllables-allow (random 4)]
          [five-syllables-allow (random 4)])
      (if (or(and (> 4 (length (list top)))
                  (> 4 (length (list bottom))))
             (and (= 7 (length middle))
                  (= seven-syllables-allow 3))
             (and (or (= 5 (length top))
                      (= 5 (length bottom))       
                      (= five-syllables-allow 3)))
             (and (= 3(length (list bottom)))
                  (= 7(length middle)))
             (and (> 5(length (list bottom)))
                  (> 7(length middle))))
          (string-append top "\n"
                         middle "\n"
                         bottom)
          (haiku-2)))))
     
;;;;;;;;;;;;;;;;;;;
;;;;; part 2
;;;;;;;;;;;;;;;;;

;;; (remove-duplicates-helper lst helper)->list
;;; lst, list?
;;; helper,list? which keeps track of recursion result so far.
;;; removes all duplicate items from a list.
(define remove-duplicates-helper
  (lambda (lst helper)
    (cond
      [(or (null? lst) (null? (cdr lst)))
       (append (reverse helper) lst) ]
      [(equal? (car lst) (cadr lst))
       (remove-duplicates-helper (cdr lst) helper)]
      [else
       (remove-duplicates-helper (cdr lst)(cons(car lst) helper))])))

;;; (remove-duplicates lst)->list?
;;; lst,list?
;;; returns implementation of remove-duplicates-helper.
(define remove-duplicates
  (lambda (lst)
    (remove-duplicates-helper lst null)))

;;; (part-of-a-word word)->list?
;;; word, string?
;;; filters out any charectors that are not part of a word.
(define part-of-a-word
  (lambda (word)
    (let ([charectors (string->list word)])
      (filter char-alphabetic? charectors))))

;;; (words->list str)->list?
;;; str, string?
;;; returns a list that contains every word in the string in order of apperance
(define words->list
  (lambda (str)
    (filter not-empty-string?
            (map list->string (map part-of-a-word (string-split str " ") )))))

;;; (unique-words str)->list?
;;; str, string?
;;; returns the list of unique words that are present in a given string. 
(define unique-words
  (lambda (str)
    (remove-duplicates (sort (filter not-empty-string? (map string-downcase (words->list str))) string-ci<?))))

;;; list of unique words that ocours in Jame Eyre
(define unique-jane (unique-words jane-string))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; part 3 count syllables
;;;;;;;;;;;;;;;;;;;;;;;;

;;; (syllable-count-single-vowel-rules str)->integer?
;;; word, string?
;;; returns number of standard vowels that appear in an english word.
;;;
;;; source used that gave syllable counting rules useing the vowel counting method
;;; link <https://www.howmanysyllables.com/howtocountsyllables>
(define syllable-count-single-vowel-rules
  (lambda (word)
    (let* ([word-to-list-of-char (string->list word)]
           [last-word-letter (last-char word)]
           [number-a (filter (section char-ci=? <> #\a) word-to-list-of-char)]
           [number-e (filter (section char-ci=? <> #\e) word-to-list-of-char)]
           [number-i (filter (section char-ci=? <> #\i) word-to-list-of-char)]
           [number-o (filter (section char-ci=? <> #\o) word-to-list-of-char)]
           [number-u (filter (section char-ci=? <> #\u) word-to-list-of-char)]
           [number-y (filter (section char-ci=? <> #\y) word-to-list-of-char)]
           [word-ends-in-e (equal? #\e last-word-letter)]
           [word-ends-in-y (equal? #\y last-word-letter)]
           [not-empty-string? (not (= (string-length word) 0))])
      (cond
        ;; case of silennt e at end of word. need to subtract 1
        [(and not-empty-string? word-ends-in-e)            
         (- (+ (length number-u) (length number-o)
               (length number-i) (length number-e)
               (length number-a))
            1)]
        ;; case of y at end of word. need to count y's as a vowel
        [ (and not-empty-string? word-ends-in-y)
          (+ (length number-y) (length number-u)
             (length number-o) (length number-i)
             (length number-e) (length number-a))] 
        [else
         (+ (length number-u) (length number-o)
            (length number-i) (length number-e)
            (length number-a))]))))
             
;;; (count-digraph-pattern lst char-first char-second)->integer?
;;; lst, list?
;;; char-first, character? first sound of digraph.
;;; char-second, character? Second sound of digraph.
;;; returns how often double vowel digraph. Occurs in list of characters.
;;;
;;; list of English digraph patterns used comes from
;;; <https://en.wikipedia.org/wiki/Diphthong#:~:text=Technically%2C%20a%20diphthong%20is%20a,diphthongs%2C%20one%20in%20every%20syllable.>
(define count-digraph-pattern
  (lambda (lst char-first char-second)
    (cond
      [(null? lst)
       0]
      [(null? (cdr lst))
       0]
      [ (and (pair? lst)
             (char-ci=? char-second (car (cdr lst)))
             (char-ci=? char-first (car lst)))
        1]
      [ else
        (count-digraph-pattern (cdr lst) char-first char-second)])))

;;; (digraph-count str)->number?
;;; str,a string?
;;; returns the number of digraphs (double vowels) are present in a given word
;;;
;;; source used < https://en.wikipedia.org/wiki/Diphthong#:~:text=Technically%2C%20a%20diphthong%20is%20a,diphthongs%2C%20one%20in%20every%20syllable.>
(define digraph-count
  (lambda (str)
    (let* ([word-to-list-char (string->list str )]
           [number-ai (count-digraph-pattern word-to-list-char #\a #\i)]
           [number-ay (count-digraph-pattern word-to-list-char #\a #\y)]
           [number-ou (count-digraph-pattern word-to-list-char #\o #\u)]
           [number-oi (count-digraph-pattern word-to-list-char #\o #\i)]
           [number-oo (count-digraph-pattern word-to-list-char #\o #\o)]
           [number-ee (count-digraph-pattern word-to-list-char #\e #\e)]
           [number-ea (count-digraph-pattern word-to-list-char #\e #\a)]) 
      (+ number-ai number-ay number-ou
         number-oi number-oo number-ee number-ea))))

;;; (count-trigraph-pattern lst char-first char-Second)->integer?
;;; lst, list?
;;; char-first, character? first sound of trigraph.
;;; char-second, character? Second sound of trigraph.
;;; char-third, character? Third sound of trigraph
;;; returns how often a specific trigraph pattern occurs in list of characters.
;;;
;;; list of trigraph patterns used comes from <https://www.theschoolrun.com/what-is-a-trigraph>
(define count-trigraph-pattern
  (lambda (lst char-first char-second char-third)  
    (cond
      [ (null? lst)
        0]
      [(null? (cdr lst))
       0]
      [ (and (pair? lst)
             (pair? (cdr (cdr lst)))
             (char-ci=? char-third (car (cdr (cdr lst))))
             (char-ci=? char-second (car (cdr lst)))
             (char-ci=? char-first (car lst)))
        1]
      [else
       (count-trigraph-pattern (cdr lst) char-first char-second char-third)])))

;;; (trigraph-count word)->integer?
;;; word, string?
;;; returns how many trigraph vowel patterns are present in a word
;;;
;;; source used <https://www.theschoolrun.com/what-is-a-trigraph>
(define trigraph-count
  (lambda (word)
    (let* ([word-to-list-char  (string->list word) ]
           [number-tch
            (count-trigraph-pattern word-to-list-char #\t #\c #\h)]
           [number-eau (count-trigraph-pattern word-to-list-char #\e #\a #\u)]
           [number-igh (count-trigraph-pattern word-to-list-char #\i #\g #\h)]
           [number-ore (count-trigraph-pattern word-to-list-char #\o #\r #\e)]
           [number-ear (count-trigraph-pattern word-to-list-char #\e #\a #\r)])
      (+ number-ear number-ore number-igh number-eau number-tch))))

;;; (cle-syllable word)->integer?
;;; word, string?
;;; returns 1 if word contains a consant l e syllable
(define cle-syllable
  (lambda (word)
    (let* ([length-word (string-length word)]
           [last-char (last-char word)]
           [second-lat-char (second-last-char word)]
           [third-last-char (third-last-char word)]
           [consant-letters (list  #\b #\c #\d #\f #\g #\h  
                                   #\j #\k #\l #\m #\n
                                   #\p #\q #\r #\s #\t 
                                   #\v #\w #\x #\y #\z)]
           [constant? (filter (section equal? third-last-char <>) consant-letters)]
           [l? (equal? second-lat-char #\l)]
           [e? (equal? last-char #\e)])
      (if (and constant? l? e?)
          1
          0))))
  
;;; (syllable-count)->integer?
;;; word, string? Of a word.
;;; returns estimated syllable count for a word.
;;;
;;; source used that described concrete syllable counting method
;;; <https://www.howmanysyllables.com/howtocountsyllables>
(define syllables
  (lambda (word)
    (let ([syllable-count-minus-cle (- (syllable-count-single-vowel-rules word)
                                       (digraph-count word)
                                       (trigraph-count word))])
      (if (<= 3 (string-length word))
          (+ (cle-syllable word) syllable-count-minus-cle)
          syllable-count-minus-cle))))

;;; (random-5-eyre)->string?
;;; returns random 5 syllable word from jane eyre
(define random-1-eyre
  (lambda ()
    (random-elt (filter (o(section = <> 1) syllables) unique-jane))))

;;; (random-5-eyre)->string?
;;; returns random 5 syllable word from jane eyre
(define random-2-eyre
  (lambda ()
    (random-elt (filter (o(section = <> 2) syllables) unique-jane))))

;;; (random-5-eyre)->string?
;;; returns random 5 syllable word from jane eyre
(define random-3-eyre
  (lambda ()
    (random-elt (filter (o(section = <> 3) syllables) unique-jane))))
;;; (random-5-eyre)->string?
;;; returns random 5 syllable word from jane eyre
(define random-4-eyre
  (lambda ()
    (random-elt (filter (o(section = <> 4) syllables) unique-jane))))

;;; (random-5-eyre)->string?
;;; returns random 5 syllable word from jane eyre
(define random-5-eyre
  (lambda ()
    (random-elt (filter (o(section = <> 5) syllables) unique-jane))))

;;; (random-7-eyre)->string?
;;; returns random 7 syllable word from jane eyre
(define random-7-eyre
  (lambda ()
    (random-elt (filter (o(section = <> 7) syllables) unique-jane))))

;;; (jane-eyere-haiku)->string?
;;; returns random geneation of jane eyere hakiku
(define jane-eyre-haiku
  (lambda ()
    (string-append (random-5-eyre) "\n"
                   (random-7-eyre) "\n"
                   (random-5-eyre))))

;;;;;;;;;;;;;;;;;;;;; part 4;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;  counting rymes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (might-rhyme-helper-2letter-words word1 word 2)-->boolean?
;;; word1, string?
;;; word2, string?
;;; returns true if one of the words is at least two letters,
;;; and matches the last two letters of the other word
(define might-rhyme-helper-2letter-words
  (lambda (word1 word2)
    (let*([length-word1 (string-length word1)]
          [length-word2 (string-length word2)]
          [last-letters-match? (equal?
                                (last-char word1)
                                (last-char word2))]
          [ second-last-letters-match? (equal? (second-last-char word1)
                                               (second-last-char word2))]  
          [rhyme? (and last-letters-match?
                       second-last-letters-match?)])
      rhyme?)))

;;; (might-rhyme-helper-atleast-3letter-words word1 word 2)-->boolean?
;;; word1, string?
;;; word2, string?
;;; returns true if one of the words is at least three letters,
;;; and matches at least the last letter of the second word.
(define might-rhyme-helper-atleast-3letter-words
  (lambda (word1 word2)
    (let*([length-word1 (string-length word1)]
          [length-word2 (string-length word2)]
          [last-letters-match (equal?  (last-char word1)
                                       (last-char word2))]
          [ second-last-letters-match? (equal? (second-last-char word2)
                                               (second-last-char word1))]
          [ third-last-letters-match? (equal? (third-last-char word1)
                                              (third-last-char word2))]
          [rhyme? (and last-letters-match
                       second-last-letters-match?
                       third-last-letters-match?)])
      rhyme?)))
  
;;; (might-rhyme? word1 word2)->boolean?
;;; word1, string?
;;; word2, string?
;;; returns true if the last three or so letters of a word match
(define might-rhyme?
  (lambda (word1 word2)
    (let ([length-word2 (string-length word2)]
          [length-word1 (string-length word1)]
          [check-last-char-equal (equal? (last-char word1)
                                         (last-char word2))])
      (cond
        [(or (= 1 length-word2)
             (= 1 length-word1))
         check-last-char-equal]     
        [(or (= 2 length-word2)
             (= 2 length-word1)) 
         (might-rhyme-helper-2letter-words word1 word2)]
        [else
         (might-rhyme-helper-atleast-3letter-words
          word1
          word2)]))))

;;; (three-letter-rhymes? word1 word2)->boolean?
;;; word1, string?
;;; word2, string?
;;; returns true if last two letters of three letter words match.
(define three-letter-rhymes?
  (lambda (word1 word2)
    (if (and (= 3 (string-length word1))
             (= 3 (string-length word2)))
        ;checks if last two letters of the word will match
        (might-rhyme-helper-2letter-words word1 word2)
        #f)))

;;; (rhyme-end-in-y word1 word2)->boolean
;;; word1, string?
;;; word2, string?
;;; returns true if word has one syllable and both ends in y for special rhyme case
;;;
;;; Source consulted in designing this rhyme exception case
;;; <https://www.rhymezone.com/r/rhyme.cgi?Word=fly&typeofrhyme=perfect&org1=syl&org2=l&org3=y>
(define rhyme-end-in-y
  (lambda (word1 word2)
    (let ([word1-ends-in-y (equal? #\y (last-char word1))]
          [word2-ends-in-y (equal? #\y (last-char word2))]
          [syllable-count-1 (= 1 (syllables word1))]
          [syllable-count-2 (= 1 (syllables word2))])
      (and word2-ends-in-y
           word1-ends-in-y syllable-count-1
           syllable-count-2))))

;;; (rhymes? word1 word2)->boolean
;;; word1, string?
;;; word2, string?
;;; a procedure that checks in general if last three letters of word matches
;;; but takes into acount exceptions for three letter words and words that end in y
(define rhymes?
  (lambda (word1 word2)
    (or (rhyme-end-in-y word1 word2)
        (three-letter-rhymes? word1 word2)
        (might-rhyme? word1 word2))))

;;; (rhymes-with word words)->list?
;;; word, string?
;;; words, a list?
;;; returns list of words that rhymes with inputted word
(define rhymes-with
  (lambda (word words)
    (filter (section rhymes? word <>) words)))

;;; (random-four-words-line words)->string?
;;; words, list?
;;; makes random line of four words
(define random-four-words-line
  (lambda (words)
    (let* ([a-1-1  (random-elt words)]
           [a-1-2  (random-elt words)]
           [a-1-3  (random-elt words)]
           [a-1-4  (random-elt words)])
      (string-append a-1-1 " " a-1-2 " "
                     a-1-3 " " a-1-4 ))))

;;; (make-random-four-words-line-rhyme words rhyme-line)->string?
;;; words, list?
;;; rhme-line, string? of text you want to rhyme to
;;; makes line of random four  words that rhmes with another line
(define make-random-four-words-line-rhyme
  (lambda (words rhyme-line)
    (let* ([a-2-1  (random-elt words)]
           [a-2-2  (random-elt words)]
           [a-2-3  (random-elt words)]
           [list-rhyme-line (words->list rhyme-line)]
           [last-word (list-ref list-rhyme-line 3)]
           [a-2-4  (random-elt (rhymes-with last-word words))])
      (string-append a-2-1 " " a-2-2 " "
                     a-2-2 " " a-2-3 " " a-2-4 ))))

;;; (abab words)->string?
;;; words, list?
;;; returns four lines of four words that follows a-b-a-b rhyme scheme
(define abab
  (lambda (words)
    (let* ([a-1 (random-four-words-line words)]
           [b-1 (random-four-words-line words)]
           [a-2  (make-random-four-words-line-rhyme words a-1)]
           [b-2 (make-random-four-words-line-rhyme words b-1)])
      (display(string-append a-1"\n" b-1"\n" a-2 "\n"b-2)))))

;;; words that fail test but  rhyme
(check-equal? (might-rhyme? "cat" "sat")
              #f "base case three letters")
(check-equal? (might-rhyme? "ran" "can")
              #f "base case three letters")
(check-equal? (might-rhyme? "sky" "fly")
              #f "base case three letters")
(check-equal? (might-rhyme? "rap" "tap")
              #f "base case three letters")
(check-equal? (might-rhyme? "pub" "rub")
              #f "base case three letters")
(check-equal? (might-rhyme? "cub" "tub")
              #f "base case three letters")
(check-equal? (might-rhyme? "rap" "cap")
              #f "base case three letters")
(check-equal? (might-rhyme? "sub" "tub")
              #f "base case three letters")
(check-equal? (might-rhyme? "cry" "fly")
              #f "base case three letters")
;;; pairs of words that pass the test and  dont rhyme
(check-equal? (might-rhyme? "conferring" "ring")
              #t "base case near ryme word")
(check-equal? (might-rhyme? "during" "ring")
              #t "base case near ryme word")
(check-equal? (might-rhyme? "tracking" "ring")
              #t "base case near ryme word")
(check-equal? (might-rhyme? "transfering" "ring")
              #t "base case near ryme word")
(check-equal? (might-rhyme? "conforming" "ring")
              #t "base case near ryme word")
(check-equal? (might-rhyme? "retrack" "black")
              #t "base case near ryme word")
(check-equal? (might-rhyme? "atack" "black")
              #t "base case near ryme word")
(check-equal? (might-rhyme? "inner" "banner")
              #t "base case three letters")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; part 5  ;;;;;;
;;;;;;;;;;;;;;;;;;;;  sentances ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (word-minus-end-punctuation str)->string?
;;; str, string?
;;; returns word without end punctuation
(define word-minus-end-puncutation
  (lambda (str)
    (substring str 0 (- (string-length str) 1))))

;;; (contain-end-puncuation? word)->boolean?
;;; word, string?
;;; returns true if statement contains one of the puncuation marks that ends a sentance
(define contain-end-puncuation?
  (lambda ( word)
    (let* ([last-char  (last-char word)]
           [end-puncuation (list #\! #\.  #\?  #\")]
           [contain-end-puncuation? (filter (section equal? last-char <>) end-puncuation)])
      (pair? contain-end-puncuation?))))
          
;;; (sentence-ends str)->list?
;;; str, string?
;;; returns list of words that ends sentences if given string has them.
(define sentence-ends
  (lambda (str)
    (let* ([string-lst (string-split str " ")]
           [rid-spaces (filter not-empty-string? string-lst)])                  
      (map word-minus-end-puncutation
           (filter contain-end-puncuation? (filter not-empty-string? rid-spaces))))))

;;; (left-neighbors-helper word so-far lst)->list?
;;; word, string?
;;; so-far, list? of recursion result.
;;; lst, list? of words with punctuation removed.                  
;;; tail recursive implementation of left-neighbors procedure
(define left-neighbors-helper
  (lambda (word so-far lst)
    (cond
      [(or (not (pair? lst))
           (null? (cdr lst)))
       (reverse so-far)]
      [(equal? word (car (cdr lst))) 
       (left-neighbors-helper word
                              (cons (list-ref lst (index-of lst (car lst))) so-far)
                              (cdr (cdr lst)))]
      [else
       (left-neighbors-helper word so-far (cdr lst))])))

;;; (left-neighbors word str)->list?
;;; word, string?
;;; str, string?
;;; returns word left to words that are immediately before given word
(define left-neighbors
  (lambda (word str)
    (left-neighbors-helper word null (words->list str))))      

;;; (right-neighbors-helper word so-far lst)->list?
;;; word, string?
;;; so-far, list? of recursion result.
;;; lst, list? of words with punctuation removed.                  
;;; tail recursive implementation of right-neighbors procedure.
(define right-neighbors-helper
  (lambda (word so-far lst)
    (cond
      [(or (null? lst)(not (pair? (cdr lst))))
       (reverse so-far)]
      [(equal? word (car lst))
       (right-neighbors-helper word
                               (cons (list-ref lst (index-of lst (car (cdr lst)))) so-far)
                               (cdr (cdr lst)))]
      [else
       (right-neighbors-helper word so-far (cdr lst))])))

;;; (right-neighbors word words)->list?
;;; word, string?
;;; words, string?
;;; returns list of words that immediately follows a  word  
(define right-neighbors
  (lambda (word words )
    (right-neighbors-helper word null
                            (words->list words))))

;;; (random-sentance-grab-word word str)->string?
;;; word, word?
;;; str, string?
;;; grabs either left or right neighbor for a specfic word
(define random-sentance-grab-word
  (lambda (word str)
    (let* ([lst1 (left-neighbors word str)]
           [lst2 (right-neighbors word str)]) 
      (cond
        [(pair? lst1) 
         (random-elt lst1)]
        [(pair? lst2)
         (random-elt lst2)]   
        [else
         null]))))

;;; (build-sentance-word-list string so-far)->list?
;;; so-far, list?
;;; string, string? of text
;;; returns six sentance word list for sentance building
(define build-sentance-word-list
  (lambda (string so-far)
    (cond
      [(null? so-far)
       (let* ([lst-a (sentence-ends string)]
              [word-a  (random-elt lst-a)])
         (build-sentance-word-list string (cons word-a so-far)))]
      [( > 6 (length so-far))
       (let* ([word (list-ref so-far 0)])
         (build-sentance-word-list string (cons (random-sentance-grab-word word string) so-far)))]
      [else
       so-far])))

;;; (captalize-first-letter word)->string?
;;; word, string?
;;; returns a word with its first letter capitalized
(define captalize-first-letter
  (lambda (word)
    (let*
        ([first-letter (char-upcase (string-ref word 0))]
         [word-minus-first-letter(substring word 1 (string-length word))]
         [convert-upercase-char-to-string (list->string (list first-letter))]
         [word-a-proper (string-append convert-upercase-char-to-string
                                       word-minus-first-letter)])
      word-a-proper)))
      
;;; (random-sentence str)->string?
;;; str, string?
;;; makes a really bad random sentence with words from the inputed string
(define random-sentence 
  (lambda (str)
    (let* ([ word-lst (build-sentance-word-list str null)]
           [cap-begining (captalize-first-letter (list-ref word-lst 0))]
           [get-word  (lambda (postion) (list-ref word-lst postion))])
                      
      (string-append cap-begining " "
                     (get-word 1) " "
                     (get-word 2) " "
                     (get-word 3) " "
                     (get-word 4) " "
                     (get-word 5) "."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; part 6 limeric form;;;;;
;;;;;;;;;;;;;;;;;;;;  counting rymes;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (random-eight-words-line words)->string?
;;; words, list?
;;; makes random line of eight words
(define random-eight-words-line
  (lambda (words)
    (let* ([a-1-1  (random-elt words)]
           [a-1-2  (random-elt words)]
           [a-1-3  (random-elt words)]
           [a-1-4  (random-elt words)]
           [a-1-5  (random-elt words)]
           [a-1-6  (random-elt words)]
           [a-1-7  (random-elt words)]
           [a-1-8  (random-elt words)])
      (string-append a-1-1 " " a-1-2 " " a-1-3 " " a-1-4 " "
                     a-1-5 " " a-1-6 " " a-1-7 " " a-1-8))))
     
;;; (make-random-eight-words-line-rhyme words rhyme-line)->string?
;;; words, list?
;;; rhme-line, string? of text you want to rhyme to
;;; makes line of random eight  words that rhmes with another line
(define make-random-eight-words-line-rhyme
  (lambda (words rhyme-line)
    (let* ([a-1-1  (random-elt words)]
           [a-1-2  (random-elt words)]
           [a-1-3  (random-elt words)]
           [a-1-4  (random-elt words)]
           [a-1-5  (random-elt words)]
           [a-1-6  (random-elt words)]
           [a-1-7  (random-elt words)]
           [list-rhyme-line (words->list rhyme-line)]
           [last-word (list-ref list-rhyme-line 7)]
           [a-1-8 (random-elt (rhymes-with last-word words))])
      (string-append a-1-1 " " a-1-2 " "a-1-3 " " a-1-4 " "
                     a-1-5 " " a-1-6 " "a-1-7 " " a-1-8))))


;;; (bad-limerick words)->string?
;;; words,list? preferably a large list of words.
;;; returns a beyond horrible limerick in the rhyme scheme of aa-bb-a.
;;;
;;; source on lymric style that was used came from
;;; <https://poets.org/glossary/limerick>
(define bad-limerick
  (lambda (words)    
    (let*
        ;; generates line 1,2,and 5  of poem
        ([a-1 (random-eight-words-line words)]
         [a-2 (make-random-eight-words-line-rhyme words a-1)]
         [b-1 (random-four-words-line words)]
         [b-2 (make-random-four-words-line-rhyme words b-1)]) 
      (string-append a-1 "\n" a-2 "\n"  b-1 "\n" b-2 "\n" a-1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; expirments and some tests 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; a really fun horrible lymric I would like to show off
#|
> (bad-limerick unique-jane)
lips lost windowi frédéric sirno groanthen belief scholarhis
wives tofriendship couldwith thenegative nameand thebrightening disclosed suchis
theworst selfdenying timeghost makingthis
andspeaking beinga beinga mistress disobeyinghis
lips lost windowi frédéric sirno groanthen belief scholarhis
> (bad-limerick unique-jane)
twilight wouldprobably seenow noan thismomentfell suppertake ofvoluntary youwould
raiment proprietornothing worsehe onlysister kneel bias thatconditionallywellwell deeperwould
giveand agoin ityeve onsaturdays
alarm ofserving ofserving forestdell holidays
twilight wouldprobably seenow noan thismomentfell suppertake ofvoluntary youwould
> (bad-limerick unique-jane)
create housemaids theirapproaching luxury readout sirvery panesputting tolerable
oppressedsuffocated eachbent inabout thefood institution grange kissedme insolvable
knowand french fabric themistress
iallowed effectwith effectwith peers hardness
create housemaids theirapproaching luxury readout sirvery panesputting tolerable
> (bad-limerick unique-jane)
meid detection darei visible function stillholding uttering carpetssuch
willquicksamyesyesyes wardrobes sown shortened thoughtful humbug ofconceiving couch
mydirection liable selfish dupethey
ignisfatuus acorner acorner bristled probablythey
meid detection darei visible function stillholding uttering carpetssuch
|#

;;; note all tests and output expirments are structured by program order
;;; so part 1 tests and expirments first .... part 6 tests and expirments apear last.

#|;; output two syllable group  expeirments
> (two-syllable-group)
"ladder"
> (two-syllable-group)
"lacrosse"
> (two-syllable-group)
"mainly"
> (two-syllable-group)
"grew ate "
> (two-syllable-group)
"laddie"|#

#| expermiental tests for three sylable group to check that it works

>  (three-syllable-group)
"Harpenden"
>  (three-syllable-group)
"blue  Dan’s Lacombe"
>  (three-syllable-group)
"safe not eighth "
>  (three-syllable-group)
"happier"
>  (three-syllable-group)
"mainful dog a "
>  (three-syllable-group)
"piece  the abbey"
>  (three-syllable-group)
"of on face "
>  (three-syllable-group)
"harmony"
>  (three-syllable-group)
"labour when But "
>  (three-syllable-group)
"abearance"|#

#|;;; output tests for four syllables
> (four-syllable-group)
"at piece  labour"
> (four-syllable-group)
"hallucinate"
> (four-syllable-group)
"blue and  law third "
> (four-syllable-group)
"paranoia"
> (four-syllable-group)
"ecodefence" |#
#|
output tests for five syllable group
> (five-syllable-group)
"some haemopoiesis"
> (five-syllable-group)
"manufacturer"
> (five-syllable-group)
"dressed good  laches looked"
> (five-syllable-group)
"kagwang abate poor"
> (five-syllable-group)
"sirs paranoia"
> (five-syllable-group)
"manipulative"
> (five-syllable-group)
"abiogenist"
> (five-syllable-group)
"little emperor"
|#

;;; output tests for seven syllable group
#|
> (seven-syllable-group)
"laddie the mainly lacis"
>  (seven-syllable-group)
"act no  ecchymosis It"
>  (seven-syllable-group)
"abbey abbreviature"
>  (seven-syllable-group)
"dog said  piece for knew Next eighth "
>  (seven-syllable-group)
"labors abdominally"
>  (seven-syllable-group)
"abduced should harbinger It"
>  (seven-syllable-group)
"Kale traditionally"
>  (seven-syllable-group)
"lacrosse mainer warm  not Bob they "
>  (seven-syllable-group)
"laddish totalitarian"
> 
|#

;;;; hakiu test experiment output
#|
> (haiku)
look seem for mainline
abbes longilateral
had abelmusk fourth
> (haiku)
manipulative
kahal in histrionics
Harlington Abba

> (haiku)
Maidstone looked dog safe
more some Aberdeenshire
Kaiser though face mainful
> (haiku)
longitudinal
abdest Dine as maguro
in this lacy not
> (haiku)
abiraterone
lactic trachelorrhaphy
Manichaeism
> (haiku)
traducianism
labors mainly harassment
lacking warm Dine deed a
> (haiku)
manufactory
dressed no aberrational
men sirs labyrinth grew But
> (haiku)
nancyboy Last too
I when piece Halkomelem
logological
> (haiku)
a harnesses out
look ate what Dine they dog law
Aberdonian
> (haiku)
marginalia
bad since though grew his eat ninth
tenth mainline so eighth
> (haiku)
happier kahal
labium abbreviature
local government
> (haiku)
act abhorrence dressed
labors dressed seem eat eighth Yet a
warm dog Magnesian
> (haiku)
tenth madarotic
mailman abecedary
luminescences
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;ouput tests for (haiku-2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;S
#|
>  (haiku-2)
ludomusical
blue as got no to no sirs what
lacewing some sick left
>  (haiku-2)
Vile dog now deed laboured
haplogy magistracy
and he Lacombe eat
>  (haiku-2)
abirritation
abbess from did ecosystem
piece left law bad sirs
>  (haiku-2)
abiraterone
mainly Mariolatrous
litterateur
>  (haiku-2)
looked tenth said this law
though piece longitudinal
tenth parenchyma
>  (haiku-2)
But lackless so some should
abbey lacer haricot
safe act abduced kaiju
>  (haiku-2)
loganamnosis
don’t out as labors abba
maiden magnified
|#
;;;; tests for remove-duplicates
 
(check-equal? (remove-duplicates '(1 2 3 4 5 9 9 8 8 0 0  )) '(1 2 3 4 5 9 8 0) "base case")
(check-equal? (remove-duplicates '( )) '() "edge check empty list is returned")
(check-equal? (remove-duplicates '( "hi" "hi")) '("hi") "edge check empty list is returned")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  tests for unique words
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-equal?  (unique-words "hi mom and dad and mom") '("and" "dad" "hi" "mom") "check with base case")
(check-equal?  (unique-words "hi mom and dad")
               '("and" "dad" "hi" "mom") "check with base case")
(check-equal? (unique-words "Hi mom I have 1 1 1 1 1 apple and apple and bannana.")
              '("and" "apple" "bannana" "have" "hi" "i" "mom") "check case with puncuation and numbers")
(check-equal? (unique-words "hello? bob! call Me  'his apple.")
              '("apple" "bob" "call" "hello" "his" "me") "edge case with more puncuation")

;;; test case for syllable count useing single vowel rules
(check-equal? (syllable-count-single-vowel-rules "hi")
              1 "base case")
(check-equal? (syllable-count-single-vowel-rules "hello")
              2 "base case")
(check-equal? (syllable-count-single-vowel-rules "fry")
              1 "edge case vowell y")
(check-equal? (syllable-count-single-vowel-rules "bcd")
              0 "edge case 0")
(check-equal? (syllable-count-single-vowel-rules "yellow")
              2 "edge case 1 with no vowel y")
(check-equal? (syllable-count-single-vowel-rules "hope")
              1 "edge case 1 with no vowel y")

;;; test used for sylllable cout
#|(check-equal? (syllable-count (random-elt five-syllable-words))
              5 "base case")|#
;;;;  tests for syllable counting useing double vowel rules
(check-equal? (digraph-count "loon") 1 "base case")
(check-equal? (digraph-count "lay") 1 "base case")
(check-equal? (digraph-count "loin") 1 "base case")
(check-equal? (digraph-count "lean") 1 "base case")
(check-equal? (digraph-count "late") 0 "edge case")

;;; tests of might rhyne function

;;; pairs words that past test and  rhyme
(check-equal? (might-rhyme? "ball" "call")
              #t "base case three letters")
(check-equal? (might-rhyme? "hall" "call")
              #t "base case three letters")
(check-equal? (might-rhyme? "craft" "raft")
              #t "base case three letters")
(check-equal? (might-rhyme? "back" "tack")
              #t "base case three letters")
(check-equal? (might-rhyme? "hack" "rack")
              #t "base case three letters")
(check-equal? (might-rhyme? "black" "tack")
              #t "base case three letters")
(check-equal? (might-rhyme? "hope" "rope")
              #t "base case three letters")
(check-equal? (might-rhyme? "sack" "rack")
              #t "base case three letters")
(check-equal? (might-rhyme? "tack" "rack")
              #t "base case three letters")
;;;;;;;;;;;;;;;
;;; rymes tests
;;;;;;;;;;;;;;;

(check-equal? (rhymes? "cat" "sat")
              #t "base case three letters")
(check-equal? (rhymes? "dog" "bog")
              #t "base case three letters")
(check-equal? (rhymes? "ring" "king")
              #t "base case four letters")
(check-equal? (rhymes? "in" "bin")
              #t "edge case diffrent amount of letters")
(check-equal? (rhymes? "rim" "rim")
              #t "edge case word being repeated")

;;; output expirments for abab
;;; note this uses small lst

#|> (abab unique-jane )
drewfrom couldoffer dayit foundryclosethey
wellknowing andtheir whenwithin british
cousinabandon sloping sloping fairfaxsparlour wordsthey
industrious evasive thesouls thesouls relish
> (abab unique-jane )
silentbecause postchaiseshortly onsleeping shrill
twoladies scholarpleased hisbodybut dormitories
mecertainlyunless great great herring till
remorse talki feelhungrythe feelhungrythe myqueries
> (abab unique-jane )
affectation emerging departedmrs wheni
reviledher theimpossibleie slenderly withindifference
afrivolous hisdisguisenow hisdisguisenow stung beeni
concern rove olivers olivers myconfidence
> (abab unique-jane )
wehad soula afrench heartmrs
clogged consoled youthank therug
owncharm aboutharvesttime aboutharvesttime chamberceilingi ofmrs
tocatch meditatedthat emerald emerald rug
> (abab unique-jane )
smiledmost welcomed thewondrous kissesbe
goodnightchapter ofsolace areused refreshedleaves
affable scrag scrag palsy heartsbe
homage yield series series resolves
> (abab unique-jane )
atan othersthat advantage overlooked
helenwas crown guineas merat
fancying toss toss methan barked
headagainst methan andwide andwide brat
> (abab unique-jane )
gavenone bachelors followedthis monthyou
benefactresss hardships arrogance crone
translation principleof principleof mused clearyou
dellby businessshe sharpers sharpers societynone
> (abab unique-jane )
unripe mewhat alwaysdid backhabitually
responsefor nom herattendant slothfuli
thebest nearlowood nearlowood belldecember wemutually
firstborn reedinterposed grecian grecian slothfuli
|#

#|
output tests for random sentance
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"The fat rat jump on that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"My hat asked the fat rat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Flat hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Jump on that Will the hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Jump on that Will the hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Is my hat How bout that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Flat hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"A flat hat asked the rat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"The cat sat on the hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"My hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Bout that brat cat The cat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Asked the hat How bout that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"The hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"My hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Rat Its now a flat hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"On that Will the fat rat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Cat sat on that brat cat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"On the rat jump on that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Hat How bout that brat cat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Rat Its now a flat hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"A flat hat How bout that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Rat Its now a flat hat."
> > (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"The fat rat jump on that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"My hat asked the fat rat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Flat hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Jump on that Will the hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Jump on that Will the hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Is my hat How bout that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Flat hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"A flat hat asked the rat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"The cat sat on the hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"My hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Bout that brat cat The cat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Asked the hat How bout that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"The hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"My hat Where is my hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Rat Its now a flat hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"On that Will the fat rat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Cat sat on that brat cat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"On the rat jump on that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Hat How bout that brat cat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Rat Its now a flat hat."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"A flat hat How bout that."
> (random-sentence  "The cat sat on the hat.  'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?  Will the fat rat jump on that brat cat?")
"Rat Its now a flat hat."
> "|#

;;; sentance ends tests
(check-equal? (sentence-ends "The cat ate the hat.  The rat sat.")
              '("hat" "sat") "base case easy sentance")

(check-equal?  (sentence-ends "Do you like blue mac and cheese?  No I don't, it makes me sneeze!")
               '("cheese" "sneeze") "another easy case")
;;; left neighbors tests
(check-equal? (left-neighbors "hat" "The cat sat on the hat. 'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?
 Will the fat rat jump on that brat cat?")
              '("the" "my" "flat") "base case 1")
(check-equal? (left-neighbors "the" "The cat sat on the hat. 'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?
 Will the fat rat jump on that brat cat?") '("on" "asked" "Will") "base case 2")

;;; right nighbors tests
(check-equal? (right-neighbors "cat" "The cat ate the hat.  The rat sat.")
              '("ate") "base case easy sentance")

(check-equal?  (right-neighbors "you" "Do you like blue mac and cheese?  No I don't, it makes me sneeze!")
               '("like") "another easy case")

(check-equal? (right-neighbors "hat" "The cat sat on the hat. 'Where is my hat?' asked the rat.  It's now a flat hat.  How 'bout that?
 Will the fat rat jump on that brat cat?")
              '("Where" "asked" "How") "base case 3")
#|
;;; output tests of bad- lymric 
> (bad-limerick unique-jane)
mediatrix valetthe harassed nowgazed barbara march enclosure an
readbut belong marshalled andmine absorbed fourthvacant andcompanyfor than
foundgratification outbefore committing steadily
controvertproduce wherethere allelse momentarily
mediatrix valetthe harassed nowgazed barbara march enclosure an
>  (bad-limerick unique-jane)
immense wanderers city thestairs causewithheld encouraged blasphemousi excepted
andblinding acquitted dontmuch andfor crimped pause audacity betrusted
flanking herdown furniturehavedisappointed none
whetherwith prolong dishedbut explanatorytone
immense wanderers city thestairs causewithheld encouraged blasphemousi excepted
>  (bad-limerick unique-jane)
somewhatcontrolling per narrowfrontdoor wellconducted thoughtmuch animpetuous mediana intimate
eldest namely voicelook burnshow led interpreterinthe selling regulate
downshe muchhave therenot discern
sawnaturehe inclinations shewill lantern
somewhatcontrolling per narrowfrontdoor wellconducted thoughtmuch animpetuous mediana intimate
>  (bad-limerick unique-jane)
toshun screenedit reflect divided blast oflate nicely elsewhere
springdrew anaccount reportedconference alonethat congeniality ambitioushe adesigning yesterdaywhere
de divided diamonds necessaryfor
anydeficiency irather strangeri gracefor
toshun screenedit reflect divided blast oflate nicely elsewhere
>  (bad-limerick unique-jane)
wasknown done thoughtno loved ofjane charmed varensenteredi openyour
changeling shethen waswintry thehousemaid varensanother woke keener mineyour
wornout thewood arched renewalof
extricating waterthe insupportable soilof
wasknown done thoughtno loved ofjane charmed varensenteredi openyour
>  (bad-limerick unique-jane)
holes forsuch savagebeautiful betheirs butfor yoursanother tease reconciliation
purchase adistinct daythat formula wretchedly thechimneys excessive union
height beenconceiving libertyto tocolloquise
fellharmless dares morethrilling advise
holes forsuch savagebeautiful betheirs butfor yoursanother tease reconciliation

|#
