#lang racket #| â˜… CSC324 Fall 2019: Exercise 7 â˜… |#
#|
Module: ex7
Description: Exercise 7: Solving Sudoku
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019

Before starting, please review the exercise guidelines at
<https://www.cs.toronto.edu/~david/csc324/homework.html>.

Please note that we *are* exporting (and testing) some of the helper
functions we've described in this lab, and not just the main algorithms.
So please make sure to read and follow the specifications carefully for *all*
functions here!

Also, you may not change choice_v3.rkt (you won't be able to submit those
files---we'll use our own for testing purposes).
|#
(provide solve-with-constraints
         with-constraints-helper
         get-constraints
         set->choice

         solve-with-ordered-constraints
         with-ordered-constraints-helper
         initialize-constraints
         sort-constraints
         update-constraints)

; (Download from Week 7 of course website)
(require "choice_v3.rkt")
(require (only-in racket/control shift reset))

;-------------------------------------------------------------------------------
; â˜… Sudoku Modeling â˜…
;-------------------------------------------------------------------------------

; We model a Sudoku board by a Racket *vector* of length 81, representing a 9x9 grid.
; Rows are stored contiguously; for example, the top row of the board is stored in
; the first 9 elements of the vector.
; Racket vectors are an array-based data structure, and provide constant-time
; indexing (unlike Racket lists).
;
; Each vector element is either a between between 1-9, representing a filled cell,
; or the number 0, representing an empty cell.
(define board-size 81)
(define 1-9 (list->set (range 1 10)))  ; The possible numbers.
(define (all-nine? s) (set=? s 1-9))   ; Whether the given set contains all numbers 1-9.

(define (blank? board i) (zero? (vector-ref board i)))  ; Whether the given cell is blank.


; Utilities for converting between a vector index and the corresponding row, column,
; and 3x3 subsquare in the Sudoku board. This numbering is all 0-indexed.
; The subsquares are numbered starting with 0 in the top-left corner,
; and increase in index first left-to-right, then top-down.
(define (to-column i) (remainder i 9))
(define (to-row i) (quotient i 9))
(define (to-subsquare i)
  (+ (quotient (to-column i) 3)
     (* 3 (quotient (to-row i) 3))))

(define (same-column? i j) (equal? (to-column i) (to-column j)))
(define (same-row? i j) (equal? (to-row i) (to-row j)))
(define (same-subsquare? i j) (equal? (to-subsquare i) (to-subsquare j)))


; Utilities for getting the set of elements in a column/row/subsquare.
(define (column board i)
  (list->set (map (lambda (j) (vector-ref board (+ i (* 9 j)))) (range 9))))

(define (row board j)
  (list->set (map (lambda (i) (vector-ref board (+ i (* 9 j)))) (range 9))))

(define (subsquare board k)
  (let* ([start-row (* 3 (quotient k 3))]
         [start-col (* 3 (remainder k 3))])
    (list->set
     (map (lambda (i)
            (vector-ref board
                        (+ (+ start-col (remainder i 3))
                           (* 9 (+ start-row (quotient i 3))))))
          (range 9)))))


; Return whether a given Sudoku board is completely solved.
; (Review the rules of Sudoku using the link on the exercise handout.)
(define (solved? board)
  (and
   ; Check columns
   (andmap (lambda (col-num) (all-nine? (column board col-num)))
           (range 9))
   ; Check rows
   (andmap (lambda (row-num) (all-nine? (row board row-num)))
           (range 9))
   ; Check subsquares
   (andmap (lambda (sub-num) (all-nine? (subsquare board sub-num)))
           (range 9))))


; Helper function for doing a non-mutating update of a board,
; analogous to list-set.
; This is pretty memory-inefficient, and is a consequence of some limitations
; of our current choice operator when mutation is concerned!
; We've provided an optional argument to turn on logging.
; This may be useful for debugging purposes, or to see how many steps your
; algorithm is taking.
(define (vector-set vec index new-item [logging #f])
  (when logging
    (displayln (format "Index: ~a Choice: ~a" index new-item)))

  (let* ([new-vec (make-vector (vector-length vec))])
    (vector-copy! new-vec 0 vec)
    (vector-set! new-vec index new-item)
    new-vec))


;-------------------------------------------------------------------------------
; â˜… A Brute Force Algorithm â˜…
;-------------------------------------------------------------------------------

#|
(solve-brute-force board) -> sudoku-board?
  board: sudoku-board?

  Returns a choice of solution for the given Sudoku board.
  See `brute-force-helper` for details.
|#
(define (solve-brute-force board) (brute-force-helper board 0))

#|
(brute-force-helper board i) -> sudoku-board?
  board: sudoku-board?
  i: integer?
    The current index to consider. Precondition: 0 <= i < 81.

  Considers each board cell one at a time (recurses on `i`).
  Each time it encounters an empty cell, this algorithm makes a *choice* among
  all 9 numbers that could fill the cell.
  It chooses a number, sets it in the vector, and moves on to the next cell.

  Only when the board is complete does this algorithm check if the board is solved;
  if it isn't, it calls (backtrack!) to backtrack to the last choice point, and tries again.
|#
(define (brute-force-helper board i)
  (cond
    ; If there are no more choices to make, check if the puzzle is actually solved.
    [(>= i (vector-length board))
     (if (solved? board)
         board
         (backtrack!))]

    ; If the current cell is occupied, move on to the next cell.
    [(not (blank? board i))
     (brute-force-helper board (+ i 1))]

    ; Else, the current cell is empty. Make a new choice!
    [else
     (let* ([choice (-< 1 2 3 4 5 6 7 8 9)]
            [new-board (vector-set board i choice)])
       (brute-force-helper new-board (+ i 1)))]))


;-------------------------------------------------------------------------------
; â˜… Task 1: Narrowing choices using constraints â˜…
;-------------------------------------------------------------------------------

#|
`solve-with-constraints` (and its corresponding helper) are almost exactly the same
as `brute-force`. The only difference is in what choices are made; rather than
using a static set of choices, the choices for each cell are generated dynamically
based on the current contents of the board.

Complete the two helpers `get-constraints` and `set->choice`, and then modify
`with-constraints-helper` to replace the (-< 1 2 3 4 5 6 7 8 9) expression.
You can change other things as well, although you shouldn't need to change much.
|#
(define (solve-with-constraints board) (with-constraints-helper board 0))

(define (with-constraints-helper board i)
  (cond
    ; If there are no more choices to make, check if the puzzle is actually solved.
    [(>= i (vector-length board))
     (if (solved? board)
         board
         (backtrack!))]

    ; If the current cell is occupied, move on to the next cell.
    [(not (blank? board i))
     (with-constraints-helper board (+ i 1))]

    ; Else, the current cell is empty. Make a new choice!
    [else
     (let* ([choice (set->choice (get-constraints board i))]
            [new-board (vector-set board i choice)])
       (with-constraints-helper new-board (+ i 1)))]))


#|
(get-constraints board i) -> set?
  board: sudoku-board?
  i: integer?
    Precondition: i is the index of an *empty cell* in `board`.

  Returns a set of the possible numbers that can fill the empty cell.
  Starts with all 9 possible numbers, and removes the numbers that are
  in the same row, column or subsquare as the given cell.

  Note: You may assume we'll only test this function for the given precondition
  on `i`. In Task 2 you may find it useful to extend the documented behaviour
  for when `i` refers to an occupied cell in the board.
|#
(define (get-constraints board i)
  (void))

#|
(set->choice set) -> any
  set: set?

  Returns a choice of an item in set, or calls (backtrack!) if the set is empty.
  Hint: the set data type has functions set-empty?, set-first, and set-rest
  that are analogous to the list functions.
|#
(define (set->choice set)
  (void))


;-------------------------------------------------------------------------------
; â˜… Task 2: Greedily ordering choices â˜…
;-------------------------------------------------------------------------------

#|
`solve-with-ordered-constraints` builds on your work in the previous task by
tackling two limitations of the previous approach:

  1. The constraints for each cell are recomputed every time backtracking occurs.
  2. The naive index-order in which the cells are considered may delay applying
     stricter constraints on later cells, leading to more choices (and hence more
     backtracking) made for the early cells.

The main helpers you'll work on here are `initialize-constraints` and
`update-constraints`, which respectively create a list of constraints for all
cells at the start of solving the puzzle, and update these constraints as
choices get made.

We've provided a helper `sort-constraints` for you that you should use to maintain
your list of constraints sorted by non-decreasing number of possibilities.
Your recursive helper will use this order to make choices, which should greatly reduce
the total number of choices made when solving Sudoku boards.
|#
(define (solve-with-ordered-constraints board)
  (void))


#|
(initialize-constraints board) -> (listof list?)
  board: sudoku-board?
    Precondition: board is solvable, which means that none of the blank cells
    will have an *empty* set of possible values.

  Returns a list of constraints for the blank cells in the given board.
  We pepresent each constraint as a list of two elements:
    - the index of the cell, and
    - a *set* containing the possible values
      that could fill the cell, using the same constraints as `get-constraints`.
|#
(define (initialize-constraints board)
  (void))


#|
(with-ordered-constraints-helper board constraints) -> sudoku-board?
  board: sudoku-board?
  constraints: (listof list?)
    A nested list of the constraints on the remaining blank cells,
    in the format described in `initialize-constraints`.

    Precondition: `constraints` is sorted first by increasing size of the set of
    possible values, and then by increasing index.

  This is the main helper for with-ordered-constraints, analogous to the previous
  two algorithms. The main difference here is the second parameter; instead of using
  an index, we use a list of the remaining constraints explicitly.

  Hints:
    - Use the same basic structure as the previous algorithms,
      though the conditions will be different.
    - Remember the basic "first and rest" recursive pattern on lists, and use it here.
|#
(define (with-ordered-constraints-helper board constraints)
  (void))


#|
(update-constraints constraints i choice) -> (listof list?)
  constraints: (listof list?)
    A nested list in the form described in `initialize-constraints`.
  i: integer?
    A valid index into a Sudoku board.
  choice: integer?
    An integer between 1-9 inclusive.

  Updates the given constraint list by adding the restriction that cell `i` is
  being given value `choice`. That is, `choice` should be removed from all
  the "possible value" sets for the indexes in the same row, column, or subsquare
  as `i`.

  You may choose to re-sort the constraints here or in the main helper above.
|#
(define (update-constraints constraints i choice)
  (void))


#|
(sort-constraints constraints) -> (listof list?)
  constraints: (listof list?)
    A nested list in the form described in `initialize-constraints`.

  Returns a sorted version of the constraints, sorting first by increasing size
  of the set of possible values, and then by increasing index.

  This function is given to you; please don't change it!
|#
(define (sort-constraints constraints)
  (sort constraints
        (lambda (a b)
          (or (< (set-count (second a)) (set-count (second b)))
              (and (equal? (set-count (second a)) (set-count (second b)))
                   (< (first a) (first b)))))))



;-------------------------------------------------------------------------------
; â˜… Demos â˜…
;-------------------------------------------------------------------------------
#|
This section includes some code for running your algorithms on actual Sudoku boards.

You can safely ignore all of this code, expect the invocations of the algorithms at
the bottom, which start off commented-out.

We took some puzzles from https://projecteuler.net/problem=96, but added our own
(very easy) puzzle at the front. See p096_sudoku.txt in the starter code.
|#

(module+ main
  (require racket/control)
  ; A puzzle file, and a function to parse it into separate puzzles.
  (define in (open-input-file "p096_sudoku.txt" #:mode 'text))

  ; Get the next puzzle from the file.
  ; Note that this is written in an imperative style; as we'll discuss later
  ; in the course, it's much harder to get away from this style when doing I/O
  ; computations.
  (define (get-next-puzzle)
    ; Check for the header line "Grid XX". If eof is found, we've reached the end of the file.
    (if (eof-object? (read-line in))
        (void)
        (let* ([nested-list-form
                (map
                 (lambda (_)
                   ; This processes a single line, converting it from a 9-letter string into a list of integers.
                   (map (lambda (c) (- (char->integer c) (char->integer #\0)))
                        (string->list (read-line in))))
                 (range 9))])
          (list->vector (apply append nested-list-form)))))

  ; A stream of Sudoku boards.
  (define (puzzle-stream)
    (let* ([puzzle (get-next-puzzle)])
      (if (void? puzzle)
          empty-stream
          (stream-cons puzzle (puzzle-stream)))))

  (define all-puzzles (stream->list (puzzle-stream)))
  (define easy (first all-puzzles))
  (define harder (second all-puzzles)))

; Run the brute force algorithm on the "easy" puzzle.
; Note that we call reset-choices! so that it doesn't interfere with subsequent choices.
#;(module+ main
    (reset (solve-brute-force easy))
    (reset-choices!))

; Run the Task 1 algorithm on the "easy" puzzle.
#;(module+ main
    (reset (solve-with-constraints easy))
    (reset-choices!))

; Run the Task 1 algorithm on the "harder" puzzle.
; Constrast this with the test before and after this!
#;(module+ main
    (reset (solve-with-constraints harder))
    (reset-choices!))

; Run the Task 2 algorithm on the "harder" puzzle.
#;(module+ main
    (reset (solve-with-ordered-constraints (fourth all-puzzles)))
    (reset-choices!))

; Can you run your algorithm on all of the puzzles in the file?
#;(module+ main
    (define all-solutions
      (map (lambda (p)
             (reset-choices!)
             (reset (solve-with-ordered-constraints p)))
           all-puzzles))
    ; This line should return #t.
    (andmap solved? all-solutions))