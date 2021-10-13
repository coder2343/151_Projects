#lang racket
(require 2htdp/image)
(require csc151)

;; CSC 151.02 (Fall 2020, Term 2)
;; PROJECT 1: Playing around_revised
;; Author: Liam Walsh
;; Date: 11-15-20
;; Acknowledgements:
;;  got help on revisons with CS tutor and Sam R for figuring out better way of building rainbow spaceship



; My overall picture is a snowman defined to be `my-snowman`.



;part 1 rainbow spaceship

;define spaceship procedure here  to cut redundance
(define block(lambda(color)
               (rectangle 100 25 'solid color)
               ))


; define 1/2 of ship collumbs because rainbow spaceship is  symetrical

(define column-1(block "red"))

(define column-2 (above column-1  (block "orange")))

(define column-3 (above  column-2 (block "yellow")))

(define column-4  (above column-3 (block "green")))

(define column-5 (above column-4 (block "blue")))

(define column-6 (above column-5 (block "violet")))


(define half-my-rainbow-spaceship (beside  column-1 column-2 column-3 column-4 column-5))

;agnonoalgement idea to implment the flip procedure came from raket doucmentation
;:https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._flip-horizontal%29%29

(define my-rainbow-spaceship (beside half-my-rainbow-spaceship column-6 (flip-horizontal half-my-rainbow-spaceship)))

#|
;a better way to reduce redunedncy when makeing my spaceship. However, can not use this because of Sam R's rules about not useing more than you know at the time.
;however just wanted to show you my bueatifuil implementation anayways
;reduce reduency by maeking half-myrainbow-spaceship into a list. Acknowledgement: got this idea from list_revisted lab's party people revisted excersize


(define half-my-rainbow-spaceship-list (list  column-1  column-2  column-3 column-4 column-5))


; assemble halfs together into one rainbow spaceship
(define my-rainbow-spaceship (beside
                              (apply beside half-my-rainbow-spaceship-list )  column-6 (apply beside (reverse half-my-rainbow-spaceship-list ))
                              ))

|#



; part 2 frestyle snowman


;body size function to make the "snowballs"

(define body (lambda(size)
               (circle size 'solid (make-color 250 250 250))
               ))


;building the medium snowball image
(define button (circle 5 'solid "red"))

;assembleing parts for the medium/midle snowball into one image
(define buttons-overlay (overlay (above button button button) (body 30)))

;subimages of the arm image
(define arm (line 2 20 "brown"))
(define finger(line 10 0 "brown"))

; procedure- for defining arm postion which joims the arm i and finger images together into a single image
(define arm-postion (lambda (orientation)
                      (overlay/align orientation 'center finger arm )
                      ))

;medium-order finishwwa assembleing pices of midle snowball into one image
(define medium-body-order(beside (arm-postion "left") buttons-overlay (arm-postion "right")))


;building small snowball image.

;this procedure outputs a tophat of a given size
(define top-hat (lambda (size color )
                  (above (rectangle size size 'solid color)(line (+ size 20) 0 color))
                  ))


;procedure that defines a face of a given size and given colors.
;output is as single image that is eyes above a nose which is a above a simley face
(define face (lambda (size eyecolor nosecolor mouthcolor)
               
               (above  (beside/align 'top  (circle size 'solid eyecolor) (circle size 'solid eyecolor))
                       
                       ;makes the clasic carrot nose
                       (triangle  (* size 2) 'solid nosecolor)
                       
                       ;makes the clasic snowman eyes
                       (ellipse (* size 4) (* size 2)'solid mouthcolor))
               ))
  

;Odering subimages of small snowball into one image that is top-hat on top of the face being overlayed on top of the small_snowball.
(define small-body-order(above
                         (top-hat 40 "black" )(overlay (face 5 "green" "red" "black") (body 20))
                         ))


 
; combines the sub_images of the snowmans body into one image

(define my-snowman( above
                    small-body-order medium-body-order (body 40)
                    ))

