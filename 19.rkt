;;; adventofcode.com/2016/day/19
; XXX using a doubly-linked list would be more efficient, but there's no
;     library for it
; NOTE there is actually no point in counting the presents
#lang racket

(require data/gvector)

;; representation of an elf participating in the present-stealing shenanigans,
;; storing its initial position in the circle (before any elves were removed)
;; and the number of presents it currently has
(struct elf (starting-position [num-presents #:mutable]))

;; returns index that follows after the given one in the given elf circle
;; (same as in a gvector, only circular)
(define (next-elf-index elf-circle index)
  (modulo (+ index 1) (gvector-count elf-circle)))

;; transfers all presents of the given victim elf to the given thief elf
(define (steal-presents! thief-elf victim-elf)
  (set-elf-num-presents!
   thief-elf (+ (elf-num-presents thief-elf) (elf-num-presents victim-elf)))
  (set-elf-num-presents! victim-elf 0))

;; returns starting position of the elf that will have all presents after the
;; present-stealing procedure is complete in a circle consisting of the given
;; number of elves, where the elf to be stolen from is determined by the given
;; "victim function"
(define (winner-elf-position num-elves victim-function)
  ;; as above, except based on current state of the circle and index of the elf
  ;; that is currently on turn (victim function is taken from the outer scope)
  (define (_winner-elf-position elf-circle elf-on-turn-index)
    (cond
      [(= (gvector-count elf-circle) 1)
       ; if the elf on turn is the only one left, it is the winner
       (elf-starting-position (gvector-ref elf-circle elf-on-turn-index))]
      [else
       ; perform one step of the present-stealing procedure: the elf on turn
       ; steals presents from the elf determined by the victim function...
       (define victim-elf-index
         (victim-function elf-circle elf-on-turn-index))
       (define victim-elf (gvector-ref elf-circle victim-elf-index))
       (define elf-on-turn (gvector-ref elf-circle elf-on-turn-index))
       (steal-presents! elf-on-turn victim-elf)
       ; ...and the robbed elf is removed from the circle
       (gvector-remove! elf-circle victim-elf-index)

       ; XXX uncomment for debug/progress info
       ;(displayln (elf-starting-position elf-on-turn))

       ; continue with next elf's turn
       (define next-elf-on-turn-index
         ; the next elf's index calculation is a bit problematic because by
         ; removing the victim elf, the elf on turn's index could have changed
         (if (< victim-elf-index elf-on-turn-index)
             (next-elf-index elf-circle (- elf-on-turn-index 1))
             (next-elf-index elf-circle elf-on-turn-index)))
       (_winner-elf-position elf-circle next-elf-on-turn-index)]))

  ; apply the helper function to the initial state: a circle consisting of
  ; the specified number of elves, each having 1 present, and the first elf
  ; currently on turn
  (define starting-elf-circle
    (list->gvector (build-list num-elves (lambda (i) (elf (+ i 1) 1)))))
  (_winner-elf-position starting-elf-circle 0))

;; solution to part one of the puzzle
(define (solution1)
  ; use next-elf-index as victim function
  (winner-elf-position 3004953 next-elf-index))

;; returns index of the elf sitting opposite in the circle to the elf at
;; the given index; if the circle has odd length, the lower index of the two
;; possible ones is returned
(define (opposite-elf-index elf-circle index)
  (define opposite-elf-distance (quotient (gvector-count elf-circle) 2))
  (modulo (+ index opposite-elf-distance) (gvector-count elf-circle)))

;; solution to part two of the puzzle
(define (solution2)
  ; use opposite-elf-index as victim function
  (winner-elf-position 3004953 opposite-elf-index))
