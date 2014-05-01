#lang racket
(require 2htdp/universe 2htdp/image)

(struct interval (small big))
(define WIDTH 450)
(define HEIGHT 200)
(define TEXT-SIZE 15)
(define TEXT-X 10)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 200)
(define SIZE 100)

;; the first help text to be displayed at the
;; top of the screen
(define HELP-TEXT
  (text "Up arrow key for larger numbers, down arrow key for smaller"
        TEXT-SIZE
        "blue"))

;; the second help text to be displayed at the
;; bottom of the screen
(define HELP-TEXT2
  ;; 
  (text "Press = when your number is pressed, press q to quite at anyt time"
        TEXT-SIZE
        "blue"))
(define COLOR "red")

;; code for the background scene
(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

;; the smaller function
(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))

;; the bigger function
(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)))

;; the guess function. essentially a basic
;; binary searching function
(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

;; handles key presses
(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down")(smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

;; the render function
;; draws the game
(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC))

;; function that renders the last scene
(define (render-last-scene w)
  (overlay (text "End" SIZE COLOR) MT-SC))

;; determines when to stop the game
(define (single? w)
  (= (interval-small w) (interval-big w)))

;; the main function
(define (start lower upper)
  (big-bang (interval lower upper)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))

(start 1 100)