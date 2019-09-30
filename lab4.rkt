#lang racket #| â˜… CSC324 Fall 2019: Lab 4 â˜… |#
#|
Module: lab4
Description: Lab 4: A Basic Object-Oriented System
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019

Lab handout: https://www.cs.toronto.edu/~david/csc324/labs/lab4/handout.html.
|#
;-------------------------------------------------------------------------------
; â˜… Task 1: Closures as objects â˜…
;-------------------------------------------------------------------------------
#|
(Point x y) -> procedure?
  x: integer?
   The x-coordinate of the point.
  y: integer?
   The y-coordinate of the point.

  Returns a function representing the *point* (x, y).
  Read the given code carefully to see how the returned function expects to
  be called, and experiment in the interpreter!
|#
(define (Point x y)
  (lambda (attr)
    (cond
      [(equal? attr 'x) x]
      [(equal? attr 'y) y]
      [else (error (format "Point has no attribute ~a." attr))])))


;-------------------------------------------------------------------------------
; â˜… Tasks 2 and 3: The attribute __dict__, and a basic functional update â˜…
;-------------------------------------------------------------------------------
#|
(PointHash x y) -> procedure?
  x: integer?
   The x-coordinate of the point.
  y: integer?
   The y-coordinate of the point.

  Task 2: Same as Point, but using a different implementation (see lab handout).
  Task 3 asks you to add one more method `set-x` to this class.
|#
(define (PointHash x y)
  (void))