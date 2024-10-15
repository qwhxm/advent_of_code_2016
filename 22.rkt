;;; adventofcode.com/2016/day/22
#lang racket

(require data/queue)
(require srfi/25) ; for multi-dimensional arrays

;; representation of a storage node
(struct node (total [used #:mutable]))

;; utility function to determine available space on a node
(define (node-available node)
  (- (node-total node) (node-used node)))

;; representation of a grid state, storing states of all storage nodes and an
;; indication of which node is currently holding the goal data
; XXX wouldn't actually need to be mutable if move-data was written better
(struct grid-state (grid goal-data-coordinates) #:mutable)

;; initial state of the storage node grid given as puzzle input
(define initial-state
  (grid-state
   (array (shape 0 34 0 30)
          (node 89 65) (node 92 65) (node 86 68) (node 90 66) (node 93 65)
          (node 90 64) (node 94 71) (node 92 68) (node 87 72) (node 86 67)
          (node 92 65) (node 94 64) (node 93 70) (node 94 66) (node 85 67)
          (node 93 66) (node 93 73) (node 93 70) (node 92 66) (node 88 72)
          (node 87 65) (node 91 70) (node 86 67) (node 94 68) (node 90 68)
          (node 92 64) (node 92 73) (node 90 72) (node 88 68) (node 90 72)
          (node 89 72) (node 88 64) (node 85 70) (node 86 64) (node 85 69)
          (node 87 73) (node 91 71) (node 85 70) (node 90 66) (node 88 71)
          (node 90 64) (node 92 70) (node 92 71) (node 88 64) (node 89 69)
          (node 93 73) (node 94 69) (node 92 72) (node 85 73) (node 94 66)
          (node 92 65) (node 92 66) (node 91 67) (node 92 64) (node 93 65)
          (node 91 72) (node 88 66) (node 89 67) (node 91 65) (node 87 65)
          (node 88 67) (node 88 65) (node 505 497) (node 89 65) (node 85 72)
          (node 85 65) (node 93 72) (node 85 69) (node 86 64) (node 91 71)
          (node 92 73) (node 91 64) (node 92 73) (node 94 64) (node 93 72)
          (node 86 65) (node 94 73) (node 85 66) (node 92 68) (node 90 69)
          (node 92 65) (node 87 66) (node 92 70) (node 88 72) (node 88 65)
          (node 87 72) (node 92 66) (node 91 65) (node 88 71) (node 94 66)
          (node 93 70) (node 87 72) (node 504 496) (node 85 70) (node 94 71)
          (node 89 71) (node 94 67) (node 85 68) (node 90 66) (node 89 69)
          (node 92 66) (node 92 68) (node 94 68) (node 93 70) (node 88 64)
          (node 92 72) (node 89 65) (node 89 70) (node 88 64) (node 89 72)
          (node 94 73) (node 88 71) (node 89 64) (node 85 72) (node 90 72)
          (node 91 71) (node 90 73) (node 92 70) (node 85 67) (node 93 71)
          (node 87 73) (node 89 73) (node 507 490) (node 92 64) (node 89 64)
          (node 86 68) (node 85 73) (node 89 65) (node 85 71) (node 89 68)
          (node 93 64) (node 85 69) (node 88 71) (node 90 69) (node 94 73)
          (node 94 64) (node 86 73) (node 88 65) (node 89 72) (node 87 73)
          (node 92 65) (node 93 72) (node 92 65) (node 90 68) (node 89 72)
          (node 91 0) (node 89 72) (node 85 72) (node 93 69) (node 85 69)
          (node 88 69) (node 86 65) (node 503 499) (node 93 71) (node 87 70)
          (node 87 65) (node 88 72) (node 93 70) (node 86 65) (node 88 73)
          (node 92 66) (node 91 68) (node 91 71) (node 87 66) (node 88 73)
          (node 86 73) (node 92 67) (node 88 72) (node 87 69) (node 94 72)
          (node 86 69) (node 86 73) (node 85 68) (node 93 69) (node 91 66)
          (node 89 65) (node 92 73) (node 85 67) (node 89 67) (node 85 66)
          (node 91 73) (node 85 73) (node 504 495) (node 87 73) (node 88 68)
          (node 93 72) (node 85 68) (node 94 72) (node 89 67) (node 93 72)
          (node 93 65) (node 89 73) (node 91 65) (node 88 72) (node 90 66)
          (node 93 68) (node 87 71) (node 88 70) (node 93 64) (node 86 69)
          (node 85 65) (node 86 65) (node 86 71) (node 93 67) (node 93 68)
          (node 94 66) (node 91 68) (node 88 71) (node 87 65) (node 85 69)
          (node 88 70) (node 88 65) (node 502 491) (node 88 72) (node 93 69)
          (node 89 73) (node 85 69) (node 90 67) (node 91 67) (node 90 65)
          (node 94 72) (node 87 64) (node 86 68) (node 86 72) (node 92 72)
          (node 94 69) (node 91 72) (node 88 72) (node 89 67) (node 94 72)
          (node 92 64) (node 89 67) (node 93 66) (node 87 71) (node 94 67)
          (node 89 67) (node 88 72) (node 87 73) (node 90 65) (node 91 65)
          (node 94 72) (node 90 64) (node 504 490) (node 92 73) (node 93 68)
          (node 88 67) (node 85 65) (node 87 73) (node 94 68) (node 85 70)
          (node 89 64) (node 87 66) (node 89 72) (node 88 64) (node 85 66)
          (node 92 70) (node 89 72) (node 92 65) (node 94 70) (node 90 68)
          (node 89 71) (node 86 65) (node 94 73) (node 90 71) (node 94 72)
          (node 88 64) (node 85 69) (node 92 69) (node 85 71) (node 93 64)
          (node 92 71) (node 88 67) (node 503 494) (node 93 73) (node 93 64)
          (node 86 72) (node 92 66) (node 94 67) (node 94 66) (node 92 72)
          (node 93 68) (node 88 65) (node 88 72) (node 90 65) (node 85 70)
          (node 91 65) (node 87 73) (node 90 72) (node 87 71) (node 85 65)
          (node 85 64) (node 85 66) (node 85 66) (node 86 66) (node 91 73)
          (node 87 70) (node 87 66) (node 90 66) (node 87 68) (node 92 71)
          (node 87 69) (node 87 66) (node 504 496) (node 89 69) (node 88 67)
          (node 94 66) (node 85 73) (node 93 68) (node 88 68) (node 92 64)
          (node 87 72) (node 89 73) (node 93 68) (node 91 71) (node 91 66)
          (node 94 69) (node 90 67) (node 88 73) (node 87 66) (node 85 72)
          (node 90 71) (node 89 65) (node 90 71) (node 87 71) (node 94 70)
          (node 90 68) (node 90 71) (node 89 70) (node 88 70) (node 91 65)
          (node 91 70) (node 94 70) (node 505 493) (node 93 72) (node 88 66)
          (node 89 71) (node 94 72) (node 93 69) (node 93 71) (node 89 67)
          (node 94 73) (node 91 65) (node 88 68) (node 92 72) (node 87 69)
          (node 92 67) (node 93 66) (node 89 69) (node 91 71) (node 86 66)
          (node 92 64) (node 85 70) (node 86 71) (node 90 67) (node 93 71)
          (node 88 70) (node 86 67) (node 94 66) (node 93 64) (node 86 67)
          (node 92 73) (node 92 66) (node 508 498) (node 88 72) (node 93 64)
          (node 86 72) (node 89 66) (node 86 67) (node 91 69) (node 93 67)
          (node 87 65) (node 94 71) (node 92 72) (node 91 70) (node 93 67)
          (node 89 71) (node 93 69) (node 93 64) (node 94 72) (node 87 65)
          (node 85 71) (node 93 72) (node 94 72) (node 92 68) (node 89 73)
          (node 85 68) (node 90 70) (node 90 72) (node 89 64) (node 85 68)
          (node 91 66) (node 92 69) (node 507 494) (node 94 68) (node 92 68)
          (node 85 66) (node 88 73) (node 85 69) (node 89 72) (node 89 68)
          (node 88 72) (node 91 69) (node 90 71) (node 88 65) (node 90 68)
          (node 86 66) (node 90 69) (node 89 65) (node 86 69) (node 92 65)
          (node 87 69) (node 94 67) (node 85 66) (node 91 68) (node 85 64)
          (node 85 71) (node 87 72) (node 88 68) (node 85 64) (node 86 70)
          (node 87 70) (node 88 66) (node 508 493) (node 92 64) (node 85 69)
          (node 94 69) (node 91 73) (node 92 70) (node 89 65) (node 94 71)
          (node 90 73) (node 85 69) (node 93 65) (node 91 70) (node 88 65)
          (node 87 72) (node 91 72) (node 86 69) (node 91 70) (node 92 73)
          (node 87 72) (node 85 72) (node 93 66) (node 92 65) (node 90 71)
          (node 85 72) (node 85 66) (node 85 72) (node 89 71) (node 91 67)
          (node 89 73) (node 86 67) (node 503 499) (node 87 72) (node 87 71)
          (node 91 70) (node 89 70) (node 86 72) (node 89 70) (node 94 67)
          (node 86 70) (node 89 64) (node 89 68) (node 94 66) (node 90 73)
          (node 88 69) (node 89 66) (node 87 69) (node 94 67) (node 90 73)
          (node 88 68) (node 85 71) (node 86 71) (node 91 73) (node 91 70)
          (node 85 72) (node 87 70) (node 86 65) (node 92 72) (node 85 66)
          (node 92 66) (node 91 64) (node 503 494) (node 94 73) (node 87 67)
          (node 90 70) (node 94 64) (node 89 71) (node 92 66) (node 89 65)
          (node 94 73) (node 93 68) (node 93 64) (node 86 65) (node 85 64)
          (node 93 72) (node 87 73) (node 86 68) (node 92 71) (node 92 65)
          (node 87 69) (node 93 72) (node 91 73) (node 92 65) (node 87 68)
          (node 90 68) (node 94 65) (node 87 67) (node 85 64) (node 89 68)
          (node 88 71) (node 89 70) (node 508 490) (node 85 72) (node 87 66)
          (node 93 73) (node 90 64) (node 86 72) (node 87 68) (node 85 72)
          (node 93 70) (node 86 67) (node 91 64) (node 85 70) (node 92 69)
          (node 90 68) (node 94 64) (node 89 71) (node 90 73) (node 87 69)
          (node 87 66) (node 92 72) (node 93 69) (node 88 68) (node 86 66)
          (node 86 72) (node 87 71) (node 86 66) (node 91 66) (node 85 64)
          (node 91 69) (node 89 69) (node 505 496) (node 88 71) (node 91 71)
          (node 91 71) (node 94 69) (node 87 70) (node 85 65) (node 90 73)
          (node 91 72) (node 92 66) (node 89 68) (node 94 68) (node 91 71)
          (node 93 70) (node 93 73) (node 87 73) (node 91 69) (node 88 71)
          (node 88 70) (node 94 69) (node 87 71) (node 88 71) (node 89 69)
          (node 93 65) (node 87 72) (node 89 66) (node 86 72) (node 86 67)
          (node 85 72) (node 86 73) (node 503 496) (node 93 69) (node 89 72)
          (node 85 64) (node 86 72) (node 87 68) (node 86 70) (node 94 64)
          (node 85 68) (node 92 64) (node 88 68) (node 90 71) (node 92 71)
          (node 93 69) (node 92 64) (node 94 64) (node 86 71) (node 86 64)
          (node 92 67) (node 87 67) (node 92 65) (node 86 71) (node 89 64)
          (node 94 72) (node 86 72) (node 88 66) (node 94 71) (node 91 64)
          (node 88 71) (node 89 70) (node 501 496) (node 92 69) (node 90 66)
          (node 92 70) (node 86 69) (node 91 70) (node 94 68) (node 93 64)
          (node 86 71) (node 94 71) (node 90 73) (node 90 69) (node 90 73)
          (node 88 67) (node 88 71) (node 87 64) (node 94 66) (node 91 72)
          (node 89 69) (node 85 66) (node 89 73) (node 85 65) (node 87 68)
          (node 94 72) (node 90 65) (node 89 71) (node 85 70) (node 85 67)
          (node 89 70) (node 92 71) (node 509 499) (node 90 70) (node 88 69)
          (node 87 72) (node 85 68) (node 88 66) (node 89 73) (node 90 64)
          (node 92 69) (node 85 73) (node 87 69) (node 94 70) (node 88 67)
          (node 89 72) (node 86 66) (node 94 70) (node 85 66) (node 87 70)
          (node 91 68) (node 85 72) (node 90 71) (node 94 70) (node 89 72)
          (node 88 70) (node 88 66) (node 94 70) (node 87 64) (node 93 64)
          (node 88 69) (node 91 65) (node 510 490) (node 92 65) (node 86 71)
          (node 85 68) (node 94 65) (node 89 72) (node 89 67) (node 93 66)
          (node 90 64) (node 90 67) (node 90 68) (node 86 68) (node 87 67)
          (node 87 64) (node 88 71) (node 88 72) (node 92 67) (node 93 71)
          (node 94 66) (node 92 70) (node 92 68) (node 87 71) (node 87 73)
          (node 89 67) (node 92 72) (node 90 69) (node 91 70) (node 85 69)
          (node 94 66) (node 90 64) (node 510 494) (node 93 71) (node 88 67)
          (node 89 69) (node 93 67) (node 93 73) (node 94 64) (node 93 64)
          (node 87 66) (node 88 64) (node 93 66) (node 90 66) (node 93 65)
          (node 87 70) (node 86 66) (node 94 72) (node 86 71) (node 93 67)
          (node 85 66) (node 86 67) (node 92 72) (node 92 69) (node 91 71)
          (node 93 68) (node 85 64) (node 90 65) (node 94 65) (node 92 68)
          (node 89 73) (node 88 70) (node 501 495) (node 87 66) (node 93 73)
          (node 85 67) (node 94 71) (node 85 64) (node 86 71) (node 86 72)
          (node 88 69) (node 93 72) (node 87 69) (node 87 72) (node 88 67)
          (node 85 73) (node 90 68) (node 91 66) (node 90 67) (node 92 69)
          (node 88 73) (node 88 71) (node 89 70) (node 86 70) (node 87 69)
          (node 85 73) (node 90 71) (node 89 71) (node 86 64) (node 91 71)
          (node 87 68) (node 93 64) (node 506 495) (node 91 69) (node 85 65)
          (node 85 67) (node 93 66) (node 90 73) (node 87 72) (node 87 64)
          (node 93 67) (node 86 71) (node 92 69) (node 89 65) (node 90 73)
          (node 93 65) (node 88 65) (node 92 71) (node 92 67) (node 93 73)
          (node 90 72) (node 94 72) (node 87 65) (node 85 68) (node 94 70)
          (node 88 65) (node 92 71) (node 88 69) (node 91 67) (node 91 69)
          (node 86 72) (node 90 69) (node 502 494) (node 89 69) (node 89 68)
          (node 88 65) (node 92 73) (node 94 64) (node 86 69) (node 90 66)
          (node 85 70) (node 90 71) (node 85 72) (node 88 72) (node 91 69)
          (node 87 73) (node 89 73) (node 86 66) (node 89 73) (node 93 67)
          (node 85 70) (node 93 66) (node 91 72) (node 88 70) (node 85 67)
          (node 91 64) (node 88 73) (node 89 64) (node 93 66) (node 86 70)
          (node 86 72) (node 88 64) (node 507 494) (node 86 71) (node 91 70)
          (node 91 69) (node 88 68) (node 93 64) (node 88 68) (node 85 72)
          (node 86 66) (node 93 68) (node 86 66) (node 92 69) (node 90 65)
          (node 88 64) (node 86 66) (node 90 67) (node 88 70) (node 94 67)
          (node 87 64) (node 86 72) (node 87 69) (node 86 69) (node 91 69)
          (node 93 70) (node 89 73) (node 88 71) (node 91 72) (node 90 68)
          (node 90 68) (node 91 68) (node 507 497) (node 86 71) (node 87 64)
          (node 92 71) (node 92 73) (node 88 73) (node 86 67) (node 88 69)
          (node 85 73) (node 92 64) (node 89 72) (node 92 65) (node 90 67)
          (node 93 65) (node 85 67) (node 93 64) (node 85 68) (node 89 73)
          (node 93 68) (node 86 72) (node 89 66) (node 88 72) (node 86 73)
          (node 90 64) (node 89 70) (node 91 71) (node 89 70) (node 91 72)
          (node 87 64) (node 93 70) (node 505 495) (node 91 66) (node 91 65)
          (node 93 71) (node 86 72) (node 92 73) (node 87 65) (node 92 72)
          (node 85 68) (node 91 70) (node 90 70) (node 92 66) (node 88 70)
          (node 91 64) (node 91 69) (node 91 73) (node 90 64) (node 85 71)
          (node 86 64) (node 94 68) (node 86 72) (node 89 70) (node 92 64)
          (node 85 70) (node 92 70) (node 85 65) (node 90 68) (node 91 67)
          (node 85 68) (node 90 73) (node 505 495) (node 91 66) (node 85 70)
          (node 86 66) (node 94 69) (node 91 69) (node 88 65) (node 93 69)
          (node 86 68) (node 86 67) (node 89 66) (node 87 72) (node 88 71)
          (node 85 65) (node 88 65) (node 92 71) (node 92 66) (node 92 73)
          (node 85 73) (node 88 66) (node 92 68) (node 93 72) (node 85 68)
          (node 94 70) (node 88 71) (node 94 71) (node 89 64) (node 85 71)
          (node 90 67) (node 87 69) (node 509 490) (node 92 64) (node 85 67)
          (node 85 64) (node 94 64) (node 89 73) (node 85 72) (node 86 70)
          (node 92 73) (node 89 68) (node 92 71) (node 91 65) (node 90 71)
          (node 93 66) (node 91 67) (node 86 72) (node 87 65) (node 86 67)
          (node 91 69) (node 90 70) (node 85 70) (node 92 70) (node 89 64)
          (node 85 73) (node 85 71) (node 89 68) (node 91 65) (node 92 72)
          (node 85 67) (node 87 68) (node 504 491) (node 94 70) (node 86 66)
          (node 91 72) (node 85 72) (node 86 67) (node 88 68) (node 85 64)
          (node 87 69) (node 85 66) (node 94 72) (node 92 69) (node 85 64)
          (node 93 68) (node 94 66) (node 94 72) (node 94 65) (node 91 67)
          (node 92 71) (node 87 65) (node 92 70) (node 94 70) (node 92 73)
          (node 87 65) (node 86 66) (node 88 72) (node 91 67) (node 92 68)
          (node 94 73) (node 89 64) (node 510 497) (node 93 72) (node 91 65)
          (node 92 67) (node 93 73) (node 92 69) (node 85 72) (node 90 72)
          (node 94 65) (node 94 72) (node 94 72) (node 87 70) (node 90 64)
          (node 94 66) (node 88 66) (node 89 66) (node 86 69) (node 92 71)
          (node 90 67) (node 90 64) (node 89 67) (node 93 73) (node 88 65)
          (node 94 69) (node 92 71) (node 93 69) (node 87 66) (node 94 68))
   (vector 33 0)))

;; determines whether the specified nodes form a viable pair in the given grid
(define (viable-pair? grid coordinates-1 coordinates-2)
  (define node-1 (array-ref grid coordinates-1))
  (define node-2 (array-ref grid coordinates-2))
  (cond
    [(= (node-used node-1) 0)
     ; node-1 is empty
     #f]
    [(equal? coordinates-1 coordinates-2)
     ; node-1 and node-2 is the same node
     #f]
    [else
     ; check if node-2 has enough space to fit data from node-1
     (<= (node-used node-1) (node-available node-2))]))

;; returns the number of viable pairs in the given grid
(define (num-viable-pairs grid)
  (define coordinates-list
    (map list->vector (cartesian-product (range (array-end grid 0))
                                         (range (array-end grid 1)))))
  (count ((curry equal?) #t)
         (for*/list ([coordinates-1 coordinates-list]
                     [coordinates-2 coordinates-list])
           (viable-pair? grid coordinates-1 coordinates-2))))

;; solution to part one of the puzzle
(define (solution1)
  (num-viable-pairs (grid-state-grid initial-state)))

; XXX the second part of this puzzle is not supposed to be solvable by a
;     straight-forward general algorithm in any reasonable time or space (see
;     reddit.com/r/adventofcode/comments/5jor9q/2016_day_22_solutions/dbhwg4l);
;     so while the below "solution" should work (*), it would need about
;     a billion years and yottabytes of memory
;     (*) after some inevitable bug fixing (e.g. I'm pretty sure equality of
;         grid states doesn't work correctly)
;;; TODO desc, successor function of state-space blah blah
;(define (successors state)
;  ;; utility function to copy a grid state
;  (define (grid-state-copy state)
;    (define grid (grid-state-grid state))
;    (define grid-copy (make-array (array-shape grid)))
;    (for* ([x (in-range (array-end grid 0))]
;           [y (in-range (array-end grid 1))])
;      (define coordinates (vector x y))
;      (define node-at-coordinates (array-ref grid coordinates))
;      (define node-at-coordinates-copy (struct-copy node node-at-coordinates))
;      (array-set! grid-copy coordinates node-at-coordinates-copy))
;    (grid-state grid-copy (grid-state-goal-data-coordinates state)))
;
;  ;; returns a list of coordinates neighbouring the given ones in the given
;  ;; grid
;  (define (neighbouring-coordinates coordinates)
;    (define x (vector-ref coordinates 0))
;    (define y (vector-ref coordinates 1))
;    (define neighbouring-coordinates-list
;      (list (vector (+ x 1) y)
;            (vector (- x 1) y)
;            (vector x (+ y 1))
;            (vector x (- y 1))))
;
;    (define (in-grid? coordinates)
;      (define grid (grid-state-grid state))
;      (define x (vector-ref coordinates 0))
;      (define y (vector-ref coordinates 1))
;      (and (and (>= x 0) (< x (array-end grid 0)))
;           (and (>= y 0) (< y (array-end grid 1)))))
;
;    (filter in-grid? neighbouring-coordinates-list))
;
;  ;; determines whether it is possible to move all data from the first to the
;  ;; second specified node (assumed to be neighbouring) in the given grid state
;  (define (can-move-data? coordinates-1 coordinates-2)
;    (define grid (grid-state-grid state))
;    (define node-1 (array-ref grid coordinates-1))
;    (define node-2 (array-ref grid coordinates-2))
;    (cond
;      [(= (node-used node-1) 0)
;       ; can't move data if there aren't any
;       #f]
;      [else
;       ; check if node-2 has enough space to fit data from node-1
;       (<= (node-used node-1) (node-available node-2))]))
;
;  ;; returns grid state obtained from the given one by moving all data from
;  ;; the first to the second specified node
;  (define (move-data coordinates-1 coordinates-2)
;    (define new-state (grid-state-copy state))
;    (define new-grid (grid-state-grid new-state))
;    (define node-1 (array-ref new-grid coordinates-1))
;    (define node-2 (array-ref new-grid coordinates-2))
;    ; move data
;    (set-node-used! node-2 (+ (node-used node-2) (node-used node-1)))
;    (set-node-used! node-1 0)
;    ; update goal data coordinates if appropriate
;    (if (equal? coordinates-1 (grid-state-goal-data-coordinates state))
;        (set-grid-state-goal-data-coordinates! new-state coordinates-2)
;        #f)
;    new-state)
;
;  ; TODO desc: for all coordinates, try to move data to all neighbouring nodes
;  (define grid (grid-state-grid state))
;  (for*/fold ([successor-list '()])
;             ([x (in-range (array-end grid 0))]
;              [y (in-range (array-end grid 1))])
;    (define coordinates (vector x y))
;    (define neighbouring-coordinates-list
;      (neighbouring-coordinates coordinates))
;    (define can-move-to-coordinates-list
;      (filter ((curry can-move-data?) coordinates)
;              neighbouring-coordinates-list))
;    ;(if (> (length can-move-to-coordinates-list) 0)
;    ;    (displayln can-move-to-coordinates-list) ;XXX
;    ;    #f)
;    (append successor-list
;            (map ((curry move-data) coordinates)
;                 can-move-to-coordinates-list))))
;
;;; TODO desc: bfs with parent pointers
;(define (shortest-path-to-target initial-state successors target?)
;  (define seen-states (mutable-set initial-state))
;  (define to-be-visited-states (make-queue))
;  (enqueue! to-be-visited-states initial-state)
;  (define predecessor-references (make-hash))
;  (hash-set! predecessor-references initial-state #f)
;
;  ;; TODO desc, returns path the search took to reach the given state
;  (define (traversed-path state)
;    (define predecessor (hash-ref predecessor-references state))
;    (cond
;      [(equal? predecessor #f)
;       ; state is the initial state
;       (list state)]
;      [else
;       (append (traversed-path predecessor) (list state))]))
;
;  ;; TODO desc, uses lot of variables from outer scopes
;  (define (_shortest-path-to-target)
;    (displayln (set-count seen-states)) ;XXX
;    ; throws if whole graph explored and no target state found
;    (define state (dequeue! to-be-visited-states))
;    (if (< (vector-ref (grid-state-goal-data-coordinates state) 0) 33)
;        (displayln (grid-state-goal-data-coordinates state)) ;XXX
;        #f)
;    (cond
;      [(target? state)
;       (traversed-path state)]
;      [else
;       (define successor-states (successors state))
;       (define unseen-successor-states
;         (filter-not ((curry set-member?) seen-states) successor-states))
;       (for ([successor-state unseen-successor-states])
;         (set-add! seen-states successor-state)
;         (enqueue! to-be-visited-states successor-state)
;         (hash-set! predecessor-references successor-state state))
;       (_shortest-path-to-target)]))
;
;  ; TODO desc
;  (_shortest-path-to-target))
;
;;; solution to part two of the puzzle
;(define (solution2)
;  (define (target? state)
;    (equal? (grid-state-goal-data-coordinates state) (vector 0 0)))
;  (length (shortest-path-to-target initial-state successors target?)))

;; prints the given grid in a form based on which the solution to part two of
;; the puzzle can be calculated by hand
(define (print-preprocessed grid)
  ;; determines whether the given node is a "wall", the "empty" node, or
  ;; an ordinary "full" node
  (define (node-type node)
    (cond
      ; "wall" node
      [(> (node-used node) 100)
       #\W]
      ; "empty" node
      [(> (node-available node) 50)
       #\e]
      ; normal "full" node
      [else
       #\.]))

  (for* ([x (in-range (array-end grid 0))]
         [y (in-range (array-end grid 1))])
    (define coordinates (vector x y))
    (define node-at-coordinates (array-ref grid coordinates))
    (printf "~a " (node-type node-at-coordinates))
    (if (= y (- (array-end grid 1) 1))
        (newline)
        #f)))

;; solution to part two of the puzzle
(define (solution2)
  ; XXX determined by hand from output of
  ;     '(print-preprocessed (grid-state-grid initial-state))' as follows:
  ;     the input grid contains exactly one node to which any data can be moved
  ;     (the "empty" node), several "wall" nodes from which no data can be
  ;     moved, and all other nodes are ordinary "full" nodes that can be made
  ;     "empty" by moving their data to the current "empty" node; the problem
  ;     then reduces to shuffling around the "empty" node (first shuffle the
  ;     "empty" node next to the node with goal data, and then shuffle the goal
  ;     data to node at coordinates (0, 0))
  220)
