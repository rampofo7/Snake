;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./snake_lib.rkt")

; a game is...
; - (make-game snake (listof posn) (listof posn) number)
; (define-struct game (snake food obstacles ticks))

; a direction is one of...
; - 'up
; - 'down
; - 'left
; - 'right
; If this type looks new to you, its just a symbol.
; That is ‘up is a symbol and “up” is a string.
; Symbols are like strings without spaces. 


; a snake is...
; - (make-snake direction (listof posn))
; (define-struct snake (heading segments))

;(make-game (make-snake 'up (list (make-posn 3 4)
                      ;(make-posn 3 5)
                    ;  (make-posn 3 6)))
           ;(list (make-posn 20 20)
                 ;(make-posn 15 10))
          ; (list (make-posn 5 1)
               ;  (make-posn 14 4))
           ;200)
                 

; segments is either
; - (cons posn empty)
; - (cons posn segments)
; That is, segments is a non-empty list of posns. 
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.

; food is either
; - empty
; - (cons posn food)
; That is, food is a list of posns.

; obstacles is either
; - empty
; - (cons posn obstacles)
; Obstacles is also a list of posns.

; add-food : game posn -> game
; Given a game and posn, returns a new game (so you want to call make-game here)
; where food has been added
; at that posn. 
(define (add-food g p)
 (make-game (game-snake g)
            (cons p (game-food g))
            (game-obstacles g)
            (game-ticks g)))

(check-expect
 (add-food (make-game (make-snake 'up (list (make-posn 1 2)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      5)
           (make-posn 6 7))
 (make-game (make-snake 'up (list (make-posn 1 2)))
            (list (make-posn 6 7) (make-posn 3 4))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            5))

; change-direction : game direction -> game
; Given a game and direction, returns a new game where the snake
;   is now headed in the provided direction. 
(define (change-direction g d)
  (make-game (make-snake d (snake-segments (game-snake g)))
             (game-food g)
             (game-obstacles g)
             (game-ticks g)))

(check-expect
 (change-direction
  (make-game (make-snake 'down (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  'left)
 (make-game (make-snake 'left (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5))

; game-score : game -> number
; Given a game, returns a score (as a number)
(define (game-score g)
  (- (* (length (snake-segments (game-snake g))) 100) (game-ticks g)))

(check-expect
 (game-score (make-game (make-snake 'up (list (make-posn 1 2)
                                              (make-posn 5 6)
                                              (make-posn 3 4)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      10))
 290)

; no tests are provided for game-score because it is open-ended
; feel free to implement it however you would like to

; game-over? : game -> boolean
; Given a game, returns true if that snake has died and false otherwise.
; We strongly recommend writing helper functions for this question!
(define (itself x)
  (member? (first (snake-segments (game-snake x)))
           (rest (snake-segments (game-snake x)))))

(define (obstacle y)
  (member? (first (snake-segments (game-snake y))) (game-obstacles y)))

(define (game-over? g)
  (or (>= (posn-x (first (snake-segments (game-snake g)))) 51)
      (>= (posn-y (first (snake-segments (game-snake g)))) 51)
      (<= (posn-x (first (snake-segments (game-snake g)))) 0)
      (<= (posn-y (first (snake-segments (game-snake g)))) 0)
      (itself g)
      (obstacle g)))

(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1))) empty empty 5))
 false)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn -1 1))) empty empty 5))
 true)

; advance-game : game -> game
; Takes a game as input and advances the game one tick. The snake
;  moves forward one segment and eats or not. 
(define (drop-last lst)
  (cond [(empty? (rest lst))      empty]
        [else           (cons (first lst) (drop-last (rest lst)))]))

(define (next-head g)
  (cond [(symbol=? 'up (snake-heading (game-snake g))) (make-posn
                                                        (posn-x (first (snake-segments
                                                                       (game-snake g))))
                                                       (+ 1 (posn-y (first (snake-segments
                                                                            (game-snake g))))))]
        [(symbol=? 'right (snake-heading (game-snake g))) (make-posn
                                                        (+ 1 (posn-x (first (snake-segments
                                                                       (game-snake g)))))
                                                      (posn-y (first (snake-segments
                                                                            (game-snake g)))))]
        [(symbol=? 'down (snake-heading (game-snake g))) (make-posn
                                                        (posn-x (first (snake-segments
                                                                       (game-snake g))))
                                                      (- (posn-y (first (snake-segments
                                                                            (game-snake g)))) 1))]
        [(symbol=? 'left (snake-heading (game-snake g))) (make-posn
                                                        (- (posn-x (first (snake-segments
                                                                       (game-snake g)))) 1)
                                                      (posn-y (first (snake-segments
                                                                            (game-snake g)))))]))
         
(define (advance-game g)
  (cond [(member (next-head g)
                 (game-food g))     (make-game
                                     (make-snake (snake-heading (game-snake g))
                                                 (cons
                                                  (next-head g) (snake-segments (game-snake g))))
                                     (remove (next-head g) (game-food g))
                                     (game-obstacles g) (+ 1 (game-ticks g)))]
        [else                       (make-game
                                     (make-snake (snake-heading (game-snake g))
                                                 (cons (next-head g)
                                                       (drop-last (snake-segments (game-snake g)))))
                                     (game-food g) (game-obstacles g) (+ 1 (game-ticks g)))]))

(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             empty
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)))
            empty
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))
(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             (list (make-posn 2 1) (make-posn 8 9))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)
                                    (make-posn 3 3)))
            (list (make-posn 8 9))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))

; a starting game to experiment with
(define game-start
  (make-game (make-snake 'up (list (make-posn 12 12)))
             (list (make-posn 2 2) 
                   (make-posn 5 20)
                   (make-posn 15 15)
                   (make-posn 24 24))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             0))

;; play : game -> game
(define (play initial-game)
  (play-game initial-game advance-game add-food change-direction game-score game-over?))

;to start a game
;(play game-start)

;to make sure all procedures are defined with no typos
(check-expect (procedure? add-food) #true)
(check-expect (procedure? change-direction) #true)
(check-expect (procedure? game-score) #true)
(check-expect (procedure? game-over?) #true)
(check-expect (procedure? advance-game) #true)