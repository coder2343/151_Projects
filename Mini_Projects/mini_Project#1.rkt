#lang racket
(require 2htdp/image)
(require csc151)


#|

feedback better formating and spacing. (comand i wont fix spacing ut it will fix indentation

fix spelling mistakes (minor things)

reduce redunency

    ex. collum 2 (above blovk red collum 1)


|#
;part 1 rainbow spaceship

;define spaceship procedure here  to cut redundance
(define block(lambda(color)
               (rectangle 100 25 'solid color)
               ))


;then define spaceshi[p collums. Only need to define 1/2 of ship becaus its symetrical

(define collum-1
  (block "red")
  )

(define collum-2 (above collum-1  (block "orange")
                        ))
(define collum-3(above  collum-2 (block "yellow")
                         ))
(define collum-4
  (above collum-3 (block "green")
                       ))
(define collum-5
  (above collum-4 (block "blue"))
  )
(define collum-6
  (above collum-5 (block "purple")))




;look up mirror procedure in documentation to shrink the rainboaw spaceship own


;reduce reduency by maeking half-myrainbow-spaceship into a list

(define half-my-rainbow-spaceship-list (list  collum-1  collum-2  collum-3 collum-4 collum-5))





(define my-rainbow-spaceship (beside (apply beside half-my-rainbow-spaceship-list )  collum-6 (apply beside (reverse half-my-rainbow-spaceship-list ))))



; part 2 frestyle snowman

; My overall picture is a snowman defined to be `my-snowman`.




;define body parts

;body size function to make the "snowballs"

(define body (lambda(size)
               (circle size 'solid (make-color 250 250 250))
               ))



;define parts of snowmans face
(define eye (circle 5 'solid "green"))

(define nose (triangle 10 'solid (make-color 237 145 33) ))

(define mouth (ellipse 20 10 'solid "black"))



;odering parts of medium snowmall
(define button(circle 5 'solid "red"))

(define medium-body (overlay (above button button button) (body 30)))

; procedure- for defining arm postion
(define arm (line 2 20 "brown"))
(define finger(line 10 0 "brown"))


(define arm-postion (lambda (orientation)(overlay/align orientation 'center finger arm )))

(define medium-order(beside (arm-postion "left") medium-body(arm-postion "right")))
  


;Odering parts of small snowball ie the face 
(define top=hat(above (rectangle 40 40 'solid "black")(line 60 0 "black")))

(define face (above  (beside/align 'top  eye eye) nose mouth))

(define small-order (above top=hat(overlay  face (body 20))))


 
; (value defining my-snowman

(define my-snowman( above small-order medium-order (body 40)))

