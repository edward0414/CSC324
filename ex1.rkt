#lang racket #| â˜… CSC324 Fall 2019: Exercise 1 â˜… |#
#|
Module: ex1
Description: Exercise 1: Getting Started with Racket and Haskell
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019

Before starting, please review the exercise guidelines at
<https://www.cs.toronto.edu/~david/csc324/homework.html>.

In this part of the exercise, you'll get started writing some simple
functions in Racket. Since this is likely your first time using Racket,
we strongly recommend going through some of the documentation we listed under
the "Software" page of the course website as you work through this exercise.
In comments below, we also give some links to documentation to built-in
functions for standard data types (numbers, strings, lists) that we want
you to become familiar with.

Finally, you'll notice the (module+ test ...) expressions interleaved with
the function definitions; this is a standard Racket convention for simple
unit tests that we'll use throughout the course. Please read them carefully,
and add tests of your own!
|#

; This specifies which functions this module exports. Don't change this!
(provide celsius-to-fahrenheit n-copies num-evens num-many-evens)

; We use (module+ test ...) to mark code that shouldn't be evaluated when this
; module is imported, but instead is used for testing purposes (similar to Python's
; if __name__ == '__main__').
;
; Right now each module+ expression after this one (i.e., the ones that actually
; contain test cases) is commented out, because they all fail, and DrRacket runs
; tests automatically when you press "Run".
; As you do your work, uncomment the module+ expression by deleting the `#;` in
; front of the module+ expressions and run this file to run the tests.
;
; NOTE: As is common to testing frameworks, by default DrRacket only displays
; output for *failing* tests. If you run the module with the tests uncommented
; but don't see any output, that's good---the tests all passed! (If you want
; to double-check this, you can try breaking test cases and seeing the "fail"
; output yourself.)
(module+ test
  ; Import the testing library
  (require rackunit))

;-------------------------------------------------------------------------------

#|
(Celsius-to-fahrenheit temp) -> integer?
  temp: number?
    A temperature in degrees Celsius.

  Returns the equivalent temperature in degrees Fahrenheit, rounded to the
  nearest integer.

  Note: number? is a predicate that returns whether its argument of a numeric type.
  The line "temp: number?" above is documentation of a type annotation, and specifies
  that the argument for `temp` should always be a number. Similarly, the "-> integer?"
  at the end of the first line indicates that this function should return an integer.
  We'll illustrate more of this style of Racket documentation---called "contracts"---
  throughout this exercise and beyond.

  Relevant documentation: https://docs.racket-lang.org/reference/generic-numbers.html.
  (Use the `round` function for rounding.)
|#
(define (celsius-to-fahrenheit temp)
  ; TODO: replace the (void) with a real implementation.
  (round (+ (* temp (/ 9 5)) 32)))

(module+ test
  ; We use rackunit's test-equal? to define some simple tests.
  (test-equal? "(celsius-to-fahrenheit 0)"  ; Test label
               (celsius-to-fahrenheit 0)    ; Actual value
               32)                          ; Expected value
  (test-equal? "(celsius-to-fahrenheit 14)"
               (celsius-to-fahrenheit 14)
               57)
  (test-equal? "(celsius-to-fahrenheit -100)"
               (celsius-to-fahrenheit -100)
               -148)
  (test-equal? "(celsius-to-fahrenheit 37)"
               (celsius-to-fahrenheit 37)
               99)
  (test-equal? "(celsius-to-fahrenheit 38)"
               (celsius-to-fahrenheit 38)
               100))

;-------------------------------------------------------------------------------
; â˜… Recursion with numbers â˜…
;-------------------------------------------------------------------------------
#|
(n-copies s n) -> string?
  s: string?
  n: natural-number/c

  Returns a string consisting of n copies of s.
  *Use recursion!* Remember that you aren't allowed to use mutation for this exercise.

  Note: `natural-number/c` is a predicate that checks whether its argument `n` is
  a non-negative integer.

  Relevant documentation: https://docs.racket-lang.org/reference/strings.html
|#
(define (n-copies s n)
  (cond
    [(equal? n 0) ""]
    [else (string-append s (n-copies s (- n 1)))]))

(module+ test
  (test-equal? "n-copies: Three copies"
               (n-copies "Hello" 3)
               "HelloHelloHello")
  (test-equal? "n-copies: Zero copies"
               (n-copies "Hello" 0)
               "")
  (test-equal? "n-copies: Single letter"
               (n-copies "a" 10)
               "aaaaaaaaaa"))

;-------------------------------------------------------------------------------
; â˜… Recursion with lists â˜…
;-------------------------------------------------------------------------------
#|
(num-evens numbers) -> integer?
  numbers: (listof integer?)  ; numbers is a list of integers

  Returns the number of even elements in the list.

  Relevant documentation: https://docs.racket-lang.org/reference/pairs.html.

  Reminder: do not use mutation or loop constructs here.
  Instead, use the basic *recursive* template on lists, which we've started
  for you in the commented code below.
|#
(define (isEven num)
  (cond
    [(even? num) 1]
    [else 0]
    )
  )
(define (num-evens numbers)
  (cond
    [(null? numbers) 0]
    [else
     ; In this case the list is non-empty. Divide the list into its first
     ; element and the remaining numbers in the list, recurse on the remaining,
     ; and combine the results in some way to return the final result.
     (let ([first-number (first numbers)])
       (+ (isEven first-number) (num-evens (rest numbers))))  ; Pick a better identifier.
     ])
  )

(module+ test
  (test-equal? "num-evens: empty list"
               (num-evens null)
               0)
  (test-equal? "num-evens: simple non-empty list"
               (num-evens (list 1 2 4))
               2))

#|
(num-many-evens lists-of-numbers)
  lists-of-numbers: (listof (listof integer?))

  Return the number of inner lists that contain three or more even integers.

  Hint: you can use a very similar approach to the previous function.
|#
(define (valid-list list)
  (cond
    [(>= (num-evens list) 3) 1]
    [else 0]
    )
  )

(define (num-many-evens lists-of-numbers)
  (cond
    [(null? lists-of-numbers) 0]
    [else
     (let ([first-list (first lists-of-numbers)])
       (+ (valid-list first-list) (num-many-evens (rest lists-of-numbers)))
       )
     ])
  )

(module+ test
    (test-equal? "num-many-evens: empty list"
                 (num-many-evens null)
                 0)
    (test-equal? "num-many-evens: simple non-empty list"
                 (num-many-evens (list (list 2 4 5 7 8)))
                 1))
