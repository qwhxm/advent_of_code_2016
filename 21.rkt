;;; adventofcode.com/2016/day/21
; NOTE brute-forcing part two instead of coding the reverse operations would
;      be easier and should be feasible (but I was too afraid it wouldn't)
#lang racket

;; scrambling operations given as puzzle input
(define operations
  '("swap position 2 with position 7" "swap letter f with letter a"
    "swap letter c with letter a" "rotate based on position of letter g"
    "rotate based on position of letter f"
    "rotate based on position of letter b" "swap position 3 with position 6"
    "swap letter e with letter c" "swap letter f with letter h"
    "rotate based on position of letter e" "swap letter c with letter b"
    "rotate right 6 steps" "reverse positions 4 through 7"
    "rotate based on position of letter f" "swap position 1 with position 5"
    "rotate left 1 step" "swap letter d with letter e" "rotate right 7 steps"
    "move position 0 to position 6" "swap position 2 with position 6"
    "swap position 2 with position 0" "swap position 0 with position 1"
    "rotate based on position of letter d" "rotate right 2 steps"
    "rotate left 4 steps" "reverse positions 0 through 2"
    "rotate right 2 steps" "move position 6 to position 1"
    "move position 1 to position 2" "reverse positions 2 through 5"
    "move position 2 to position 7" "rotate left 3 steps"
    "swap position 0 with position 1" "rotate based on position of letter g"
    "swap position 5 with position 0" "rotate left 1 step"
    "swap position 7 with position 1" "swap letter g with letter h"
    "rotate left 1 step" "rotate based on position of letter g"
    "reverse positions 1 through 7" "reverse positions 1 through 4"
    "reverse positions 4 through 5" "rotate left 2 steps"
    "swap letter f with letter d" "swap position 6 with position 3"
    "swap letter c with letter e" "swap letter c with letter d"
    "swap position 1 with position 6" "rotate based on position of letter g"
    "move position 4 to position 5" "swap letter g with letter h"
    "rotate based on position of letter h" "swap letter h with letter f"
    "swap position 3 with position 6" "rotate based on position of letter c"
    "rotate left 3 steps" "rotate based on position of letter d"
    "swap position 0 with position 7" "swap letter e with letter d"
    "move position 6 to position 7" "rotate right 5 steps"
    "move position 7 to position 0" "rotate left 1 step"
    "move position 6 to position 2" "rotate based on position of letter d"
    "rotate right 7 steps" "swap position 3 with position 5"
    "move position 1 to position 5" "rotate right 0 steps"
    "move position 4 to position 5" "rotate based on position of letter b"
    "reverse positions 2 through 4" "rotate right 3 steps"
    "swap letter b with letter g" "rotate based on position of letter a"
    "rotate right 0 steps" "move position 0 to position 6"
    "reverse positions 5 through 6" "rotate left 2 steps"
    "move position 3 to position 0" "swap letter g with letter b"
    "move position 6 to position 1" "rotate based on position of letter f"
    "move position 3 to position 2" "reverse positions 2 through 7"
    "swap position 0 with position 4" "swap letter e with letter b"
    "rotate left 4 steps" "reverse positions 0 through 4"
    "rotate based on position of letter a"
    "rotate based on position of letter b" "rotate left 6 steps"
    "rotate based on position of letter d" "rotate left 7 steps"
    "swap letter c with letter d" "rotate left 3 steps"
    "move position 4 to position 6" "move position 3 to position 2"
    "reverse positions 0 through 6"))

;; utility string-to-vector/vector-to-string conversion functions
(define (string->vector string)
  (list->vector (string->list string)))

(define (vector->string vector)
  (list->string (vector->list vector)))

;; utility functions to perform the individual scrambling operations; all take
;; password as a vector and return the result as (possibly another) vector
(define (swap-positions password i j)
  (define character-at-i (vector-ref password i))
  (define character-at-j (vector-ref password j))
  (vector-set! password i character-at-j)
  (vector-set! password j character-at-i)
  password)

(define (swap-characters password x y)
  (define position-of-x (vector-member x password))
  (define position-of-y (vector-member y password))
  (swap-positions password position-of-x position-of-y))

(define (rotate-by-steps password direction n)
  (define n-modulo-password-length (modulo n (vector-length password)))
  (define split-at-position
    (cond
      [(equal? direction "left")
       n-modulo-password-length]
      [(equal? direction "right")
       (- (vector-length password) n-modulo-password-length)]))
  (define-values (part-1 part-2) (vector-split-at password split-at-position))
  (vector-append part-2 part-1))

(define (rotate-by-character-position password x)
  (define position-of-x (vector-member x password))
  (define rotation-steps
    (if (>= position-of-x 4)
        (+ position-of-x 2)
        (+ position-of-x 1)))
  (rotate-by-steps password "right" rotation-steps))

(define (reverse-between-positions password i j)
  (define before-part (vector-take password i))
  (define after-part (vector-drop password (+ j 1)))
  (define reversed-part
    (list->vector (reverse (vector->list (vector-copy password i (+ j 1))))))
  (vector-append before-part reversed-part after-part))

(define (move-position password i j)
  (define character-at-i (vector-ref password i))
  (if (< i j)
      (for ([k (in-range i j)])
        (vector-set! password k (vector-ref password (+ k 1))))
      (for ([k (in-range i j -1)])
        (vector-set! password k (vector-ref password (- k 1)))))
  (vector-set! password j character-at-i)
  password)

;; returns the result of applying the given scrambling operation to the given
;; password
(define (apply-operation operation password)
  (match operation
    [(regexp #px"swap position (\\d+) with position (\\d+)" (list _ i j))
     (swap-positions password (string->number i) (string->number j))]
    [(regexp #px"swap letter (\\w) with letter (\\w)" (list _ x y))
     (swap-characters password (string-ref x 0) (string-ref y 0))]
    [(regexp #px"rotate (\\w+) (\\d+) steps?" (list _ direction n))
     (rotate-by-steps password direction (string->number n))]
    [(regexp #px"rotate based on position of letter (\\w)" (list _ x))
     (rotate-by-character-position password (string-ref x 0))]
    [(regexp #px"reverse positions (\\d+) through (\\d+)" (list _ i j))
     (reverse-between-positions
      password (string->number i) (string->number j))]
    [(regexp #px"move position (\\d+) to position (\\d+)" (list _ i j))
     (move-position password (string->number i) (string->number j))]))

;; returns the result of applying the given list of scrambling operations to
;; the given password
(define (apply-operations operation-list password)
  (foldl apply-operation password operation-list))

;; solution to part one of the puzzle
(define (solution1)
  (vector->string (apply-operations operations (string->vector "abcdefgh"))))

;; utility functions to reverse the individual scrambling operations; all take
;; password as a vector and return the result as (possibly another) vector
(define (reverse-swap-positions password i j)
  ; swap-positions is an involution
  (swap-positions password i j))

(define (reverse-swap-characters password x y)
  ; swap-characters is an involution
  (swap-characters password x y))

(define (reverse-rotate-by-steps password direction n)
  ; to reverse a rotation, rotate in the opposite direction
  (define opposite-direction
    (cond
      [(equal? direction "left") "right"]
      [(equal? direction "right") "left"]))
  (rotate-by-steps password opposite-direction n))

(define (reverse-rotate-by-character-position password x)
  ; XXX to reverse this operation, we have to assume that the password has
  ;     length 8, because (only?) then it is possible to unambiguously
  ;     determine at what position the given letter was before the operation
  (define new-position-of-x (vector-member x password))
  (define old-position-of-x
    (cond
      ; lookup table calculated by hand
      [(= new-position-of-x 1) 0]
      [(= new-position-of-x 3) 1]
      [(= new-position-of-x 5) 2]
      [(= new-position-of-x 7) 3]
      [(= new-position-of-x 2) 4]
      [(= new-position-of-x 4) 5]
      [(= new-position-of-x 6) 6]
      [(= new-position-of-x 0) 7]))
  ; rotate to the left by the number of steps necessary to "return" the
  ; character to its old position
  (define rotation-steps (- new-position-of-x old-position-of-x))
  (rotate-by-steps password "left" rotation-steps))

(define (reverse-reverse-between-positions password i j)
  ; reverse-between-positions is an involution
  (reverse-between-positions password i j))

(define (reverse-move-position password i j)
  ; to reverse a move, move in the opposite direction
  (move-position password j i))

;; returns the result of reversing the given scrambling operation that was
;; applied to the given password
(define (reverse-operation operation password)
  (match operation
    [(regexp #px"swap position (\\d+) with position (\\d+)" (list _ i j))
     (reverse-swap-positions password (string->number i) (string->number j))]
    [(regexp #px"swap letter (\\w) with letter (\\w)" (list _ x y))
     (reverse-swap-characters password (string-ref x 0) (string-ref y 0))]
    [(regexp #px"rotate (\\w+) (\\d+) steps?" (list _ direction n))
     (reverse-rotate-by-steps password direction (string->number n))]
    [(regexp #px"rotate based on position of letter (\\w)" (list _ x))
     (reverse-rotate-by-character-position password (string-ref x 0))]
    [(regexp #px"reverse positions (\\d+) through (\\d+)" (list _ i j))
     (reverse-reverse-between-positions
      password (string->number i) (string->number j))]
    [(regexp #px"move position (\\d+) to position (\\d+)" (list _ i j))
     (reverse-move-position password (string->number i) (string->number j))]))

;; returns the result of reversing the given list of scrambling operations that
;; were applied to the given password
(define (reverse-operations operation-list password)
  (foldr reverse-operation password operation-list))

;; solution to part two of the puzzle
(define (solution2)
  (vector->string (reverse-operations operations (string->vector "fbgdceah"))))
