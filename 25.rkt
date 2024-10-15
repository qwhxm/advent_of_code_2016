;;; adventofcode.com/2016/day/25
#lang racket

;; assembunny code given as puzzle input
(define code
  #("cpy a d" "cpy 7 c" "cpy 362 b" "inc d" "dec b" "jnz b -2" "dec c"
    "jnz c -5" "cpy d a" "jnz 0 0" "cpy a b" "cpy 0 a" "cpy 2 c" "jnz b 2"
    "jnz 1 6" "dec b" "dec c" "jnz c -4" "inc a" "jnz 1 -7" "cpy 2 b" "jnz c 2"
    "jnz 1 4" "dec b" "dec c" "jnz 1 -4" "jnz 0 0" "out b" "jnz a -19"
    "jnz 1 -21"))

;; state of an assembunny program during interpretation, consisting of program
;; code (vector of instructions), contents of registers (hash table), next
;; expected signal value (0 or 1), and program counter
(struct program-state (code registers next-signal-value pc) #:mutable)

;; utility functions to manipulate program state
(define (code-ref state index)
  (vector-ref (program-state-code state) index))

(define (code-set! state index instruction)
  (vector-set! (program-state-code state) index instruction))

(define (registers-ref state register)
  (hash-ref (program-state-registers state) register))

(define (registers-set! state register value)
  (hash-set! (program-state-registers state) register value))

(define (flip-next-signal-value! state)
  (define new-next-signal-value
    (modulo (+ (program-state-next-signal-value state) 1) 2))
  (set-program-state-next-signal-value! state new-next-signal-value))

(define (increment-pc! state offset)
  (set-program-state-pc! state (+ (program-state-pc state) offset)))

;; utility functions to parse assembunny instructions
(define instruction-regexp
  #rx"(cpy|inc|dec|jnz|tgl|out) (-?[0-9]+|[a-d]) ?(-?[0-9]+|[a-d])?")

(define (type-of instruction)
  (list-ref (regexp-match instruction-regexp instruction) 1))

(define (arg1-of instruction)
  (list-ref (regexp-match instruction-regexp instruction) 2))

(define (arg2-of instruction)
  (list-ref (regexp-match instruction-regexp instruction) 3))

(define (args-of instruction)
  (substring instruction 4))

;; utility functions to interpret instruction arguments
(define (arg->register arg)
  (string->symbol arg))

(define (arg->value arg state)
  (cond
    [(regexp-match? #rx"-?[0-9]+" arg)
     ; constant integer value
     (string->number arg)]
    [(regexp-match? #rx"[a-d]" arg)
     ; value stored in a register
     (registers-ref state (arg->register arg))]))

;; utility functions to execute the individual assembunny instructions;
;; all take current program state and modify it to the result state
(define (cpy arg1 arg2 state)
  (define value-to-copy (arg->value arg1 state))
  (define target-register (arg->register arg2))
  (registers-set! state target-register value-to-copy)
  (increment-pc! state 1))

(define (inc arg1 state)
  (define register-to-increment (arg->register arg1))
  (define current-value (arg->value arg1 state))
  (registers-set! state register-to-increment (+ current-value 1))
  (increment-pc! state 1))

(define (dec arg1 state)
  (define register-to-decrement (arg->register arg1))
  (define current-value (arg->value arg1 state))
  (registers-set! state register-to-decrement (- current-value 1))
  (increment-pc! state 1))

(define (jnz arg1 arg2 state)
  (define condition-value (arg->value arg1 state))
  (define jump-offset (arg->value arg2 state))
  (if (not (= condition-value 0))
      (increment-pc! state jump-offset)
      (increment-pc! state 1)))

(define (tgl arg1 state)
  (define toggle-offset (arg->value arg1 state))
  (define index-to-toggle (+ (program-state-pc state) toggle-offset))
  (define instruction-to-toggle (code-ref state index-to-toggle))
  (define new-instruction
    (string-append (match (type-of instruction-to-toggle)
                     ["cpy" "jnz"]
                     ["inc" "dec"]
                     ["dec" "inc"]
                     ["jnz" "cpy"]
                     ["tgl" "inc"]
                     ["out" "inc"])
                   " "
                   (args-of instruction-to-toggle)))
  (code-set! state index-to-toggle new-instruction)
  (increment-pc! state 1))

(define (out arg1 state)
  (define signal-value (arg->value arg1 state))
  (display signal-value)
  (cond
    [(= signal-value (program-state-next-signal-value state))
     (flip-next-signal-value! state)
     (increment-pc! state 1)]
    [else
     ; wrong signal value, terminate the program
     (newline)
     (set-program-state-pc! state -1)]))

;; runs the given assembunny program (specified by initial code and register
;; contents) and returns contents of registers after the program terminates
(define (run initial-code initial-registers)
  ; prepare initial program state
  (define state (program-state initial-code initial-registers 0 0))

  ; interpret instructions until the program counter moves out of range of
  ; the program
  (for ([_ (in-naturals)])
    (define code (program-state-code state))
    (define pc (program-state-pc state))

    ; check for termination
    #:break (or (< pc 0) (>= pc (vector-length code)))

    ; execute the current instruction; if this causes an exception to be
    ; raised, consider the instruction invalid and skip it (this works
    ; correctly because errors always manifest before state is modified)
    (define instruction (vector-ref code pc))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (displayln (string-append "skipping " instruction))
                       (increment-pc! state 1))])
      (match (type-of instruction)
        ["cpy" (cpy (arg1-of instruction) (arg2-of instruction) state)]
        ["inc" (inc (arg1-of instruction) state)]
        ["dec" (dec (arg1-of instruction) state)]
        ["jnz" (jnz (arg1-of instruction) (arg2-of instruction) state)]
        ["tgl" (tgl (arg1-of instruction) state)]
        ["out" (out (arg1-of instruction) state)])))

  ; return final register contents
  (program-state-registers state))

;; solution to part one of the puzzle
(define (solution1)
  ; try running the given code with register a initialized to increasing
  ; integers, until one is found that results in signal output 010101...,
  ; repeating infinitely; the function will never return, but will print
  ; the correct solution
  (for ([i (in-naturals 1)])
    (displayln (string-append "trying " (number->string i) ":"))
    (define initial-code (vector-copy code))
    (define initial-registers (make-hash [map cons '(a b c d) (list i 0 0 0)]))
    (run initial-code initial-registers)))

;; solution to part two of the puzzle
(define (solution2)
  #t)
