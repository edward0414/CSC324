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
      [(equal? attr 'scale) (lambda (num) (Point (* x num) (* y num)))]
      [else (error (format "Point has no attribute ~a." attr))])))

(define (point-sum func)
  (+ (func 'x) (func 'y)))

(point-sum (Point 2 3))

(define (point-dist pt1 pt2)
  (sqrt (+ (expt (- (pt1 'x) (pt2 'x)) 2) (expt (- (pt1 'y) (pt2 'y)) 2))))

(point-dist (Point 2 2) (Point 5 6))

(define (generate-pt num)
  (map (lambda (i) (Point i i)) (range num)))

(generate-pt 5)

(define p (Point 2 3))      ; p is the point (2, 3)
(p 'scale)                  ; #<procedure...>

(define p2 ((p 'scale) 3))  ; p2 is the point (6, 9)
(p2 'x)
(p2 'y)

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
  (lambda (attr)
    (let ([__dict__ (hash 'x x 'y y 'scale (lambda (i) (PointHash (* x i) (* y i))) 'set (lambda (attr i) (cond [(equal? attr 'x) (PointHash i y)] [(equal? attr 'y) (PointHash x i)])))])
      (hash-ref __dict__ attr))))