#lang racket #| â˜… CSC324 Fall 2019: Week 4 Example Code â˜… |#

; This file contains some extra examples of macro ellipses in action.
; Please review this carefully, and make sure you understand the examples
; at the bottom of this file.

(define-syntax ellipsis-1
  (syntax-rules ()
    [(ellipsis-1 <attr> ...)
     (list <attr> ...)]))

(define-syntax ellipsis-2
  (syntax-rules ()
    [(ellipsis-2 (<a> <b>) ...)
     (list (+ <a> <b>) ...)]))

(define-syntax ellipsis-3
  (syntax-rules ()
    [(ellipsis-3 (<a> ...) ...)
     (list (+ <a> ...) ...)]))


; Uncomment each one, and try running it through the Macro Stepper!
(ellipsis-1 10 "Hello" 'david #f)
(ellipsis-2 (10 20) (30 40) (-1 -1))
(ellipsis-3 (10 20) (324) () (1 2 3 4 5 6 7))