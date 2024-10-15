;;; adventofcode.com/2016/day/18
#lang racket

;; first row of the floor map, given as puzzle input
(define first-row
  (list->vector (string->list ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^")))

;; based on the given row, generates the row following it
(define (next-row previous-row)
  ;; determines type of tile in the given column of the row following the given
  ;; one
  (define (tile-in-next-row previous-row column-index)
    (define left
      (if (= column-index 0)
          #\.
          (vector-ref previous-row (- column-index 1))))
    (define center
      (vector-ref previous-row column-index))
    (define right
      (if (= column-index (- (vector-length previous-row) 1))
          #\.
          (vector-ref previous-row (+ column-index 1))))
    (cond
      [(and (equal? left #\^)
            (equal? center #\^)
            (equal? right #\.))
       #\^]
      [(and (equal? left #\.)
            (equal? center #\^)
            (equal? right #\^))
       #\^]
      [(and (equal? left #\^)
            (equal? center #\.)
            (equal? right #\.))
       #\^]
      [(and (equal? left #\.)
            (equal? center #\.)
            (equal? right #\^))
       #\^]
      [else
       #\.]))

  (for/vector ([column-index (in-range (vector-length previous-row))])
    (tile-in-next-row previous-row column-index)))

;; returns the number of safe tiles in floor map starting with the given row
;; and continuing to contain the specified number of rows
(define (safe-tiles-in-floor starting-row num-rows)
  ;; counts the number of safe tiles in the given row
  (define (safe-tiles-in-row row)
    (vector-count ((curry equal?) #\.) row))

  ; generate the specified number of rows and count safe tiles in them
  ; XXX always generates one more row than needed, could be rewritten not to
  (cond
    [(= num-rows 0)
     0]
    [else
     (+ (safe-tiles-in-row starting-row)
        (safe-tiles-in-floor (next-row starting-row) (- num-rows 1)))]))

;; solution to part one of the puzzle
(define (solution1)
  (safe-tiles-in-floor first-row 40))

;; solution to part two of the puzzle
(define (solution2)
  (safe-tiles-in-floor first-row 400000))
