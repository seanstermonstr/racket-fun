#lang racket
(require 2htdp/universe 2htdp/image)
(define WIDTH 500)
(define HEIGHT 500)
(define SIZE 30)

;; tick rate
(define TICK-RATE 1/10)
;; snake constant
(define SEG-SIZE 15)
;; goo constants
(define MAX-GOO 5)
(define EXPIRATION-TIME 150)

;; graphical board
(define WIDTH-PX (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

;; Visual Constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "goo.gif"))
(define SEG-IMG (bitmap "body.gif"))
(define HEAD-IMG (bitmap "head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)



(struct pit (snake goos) #:transparent)
(struct snake (dir segs) #:transparent)
(struct posn (x y) #:transparent)
(struct goo (loc expire) #:transparent)



;; main function
(define (start-snake)
  ;sets it all in motion with creating a pit object
  ;to initialize the game-state
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

;; main rendering expression
(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

;;draws the snake onto the scene
(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir(snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

;; helper function for drawing or something
;; calls img-scene
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

;; another helper function that determines position and places the image
(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

;; draws the goo
(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

;; checks if the snake is dead
(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

;; checks if the snake if colliding with itself
(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

;; checks if the snake collides with the wall
(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

;; determines if a goo is rotten
(define (rotten? g)
  (zero? (goo-expire g)))

;; creates a fresh goo
;; why do we need add1 here?
;; also sub1? also what do those do?
(define(fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))
  
;; renews the goos that need it by recursively calling itself
(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goo)))]
        [else
         (cons(first goos) (renew (rest goos)))]))
  
;; rots the goos
;; gets goos as a list of goos
(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

;; Goo -> Goo
;; decreases the expire field of goo by one
;; > (decay (goo (posn 1 2) 2))
;; (goo (posn 1 2) 1)
(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))

;; deals with the rotting goo
;; expressed as a composition of rot and renew functions
;; rot takes the result of renew as an argument
(define (age-goo goos)
  (rot (renew goos)))

;; function determines if a goo is close enough to eat
(define (close? s g)
  (posn=? s (goo-loc g)))

;; function that handles the snake eating the goo
(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

;; function that grown the snake
(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

;; determines the new location of the head
(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))
  
;; moves the head of the snake
(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

;; handles the slithering
(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

;; returns the given list of segments without the last one
(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))
  

;; function to determine if the snake can eat
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
              (first goos)
              (can-eat snake (rest goos)))]))

;; function to handle clock ticks
(define (next-pit w)
  (define snake (pit-snake w)) ; extracts snake from pit struct
  (define goos (pit-goos w)) ; extracts goos from pit struct
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      ; grows the snake, eats the goo
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      ; else slithers the snake ages the goo
      (pit (slither snake) (age-goo goos))))

;; checks if the new input sends snake in the opposite direction
(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down?")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

;; changes the direction of the snake in responce to the keystroke
(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d)
              ;; consists of the head and at least one segment
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else 
         (pit (snake-change-dir the-snake d) (pit-goos w))]))

;; gets the direction...
(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

;; directs the snake, key handler function
(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

;; checks if the game is over
(define (render-end w)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

;; determines if two positions have the same x and y field
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;;
(define (snake-head sn)
  (first (snake-segs sn)))

;;
(define (snake-body sn)
  (rest (snake-segs sn)))

;;
(define (snake-tail sn)
  (last (snake-segs sn)))

;;
(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))







