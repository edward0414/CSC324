#lang racket #| â˜… CSC324 Fall 2019: Lab 8 â˜… |#
#|
Module: lab8
Description: Lab 8: More with the Ambiguous Operator
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019

Lab handout: https://www.cs.toronto.edu/~david/csc324/labs/lab8/handout.html.
|#
(require "choice_v3.rkt")

;-------------------------------------------------------------------------------
; â˜… Task 1: Generalizing expression generation â˜…
;-------------------------------------------------------------------------------
#|
(expression-of-rank-gen grammar k) -> string?
  grammar: list?
    A list containing two thunks, where the first thunk returns a choice
    of an atom in the grammar, and the second returns a choice of a rule
    in the grammar.
  k: (and/c integer? (not/c negative?))

  Returns a (choice of) binary expression of rank k from the grammar.
|#
(define (expression-of-rank-gen grammar k)
  (void))


; Your function should work for the following input grammars.
(define arithmetic-grammar
  (list
   (thunk (-< 1 2 3 4))
   (thunk (-< (lambda (e1 e2) (list '+ e1 e2))
              (lambda (e1 e2) (list '* e1 e2))))))
(define life-choices-grammar
  (list
   (thunk (-< "cats" "dogs" "birds" "love" "terror" "hunger"))
   (thunk (-< (lambda (e1 e2) (format "~a and ~a!" e1 e2))
              (lambda (e1 e2) (format "~a or ~a?" e1 e2))))))

;-------------------------------------------------------------------------------
; â˜… Task 2: Making change â˜…
;-------------------------------------------------------------------------------
#|
(make-change n) -> (listof integer?)
  n: (and/c integer? (not/c negative?))

  Returns a choice of a list of 1's and 5's whose sum is n. The returned list
  should be in *non-increasing order*, i.e., all 5's should appear before all 1's.

  Note: we strongly recommend writing a helper function that takes a number and
  returns a list of 1's of that length. This will prepare you to do the recursive
  thinking required to generalize `make-change` later in the lab.

  You might want to lookup the "build-list" function.
|#
(define (make-change n)
  (void))


(module+ test
  (require rackunit)

  (test-equal? "make-change 0"
               (list->set (stream->list (generate (make-change 0))))
               (set null))
  (test-equal? "make-change 1"
               (list->set (stream->list (generate (make-change 1))))
               (set (list 1)))
  (test-equal? "make-change 5"
               (list->set (stream->list (generate (make-change 5))))
               (set (list 5) (list 1 1 1 1 1)))
  (test-equal? "make-change 13"
               (list->set (stream->list (generate (make-change 13))))
               (set (list 5 5 1 1 1)
                    (list 5 1 1 1 1 1 1 1 1)
                    (list 1 1 1 1 1 1 1 1 1 1 1 1 1))))


#|
(make-change-gen coins n) -> (listof integer?)
  coins: (listof integer?)
    A list of distinct positive integers, sorted in decreasing order.
  n: integer?

  Returns a choice of a list of numbers in `coins` whose sum is `n`.
  The list should be sorted in non-increasing order.
  As before, coin values may be used more than once.

  If no possible combinations in `coins` sums to n (including when n is negative),
  call (backtrack!).
  But return an *empty list* if n = 0---this is a different case!

  Notes:
    1. We have started the pattern-matching for you, and strongly recommend building
       off of these patterns.
    2. You might need to have the output choices come out in a different order
       than in `make-change`. That's fine!
|#
(define/match (make-change-gen coins n)
  ; If n = 0, return an empty list.
  [(_ 0) (void)]

  ; If n < 0, backtrack (it's impossible to get a negative coin total).
  [(_ (? negative?)) (void)]
  
  ; If there are no more coins to choose from, backtrack.
  [((list) _) (void)]

  ; If there's just one coin value, think about what to do here!
  [((list coin) _) (void)]

  ; If there's more than one coin value, choose one of:
  ;   - a combination of (rest coins) whose sum is n
  ;   - a combination of coins whose sum is (- n (first coins)), together with
  ;     an occurrence of (first coins)
  [((cons first-coin rest-coins) _) (void)])


#;(module+ test
  (test-equal? "make-change-gen (10 3) 13"
               (list->set (stream->list (generate (make-change-gen (list 10 3) 13))))
               (set (list 10 3)))

  (test-equal? "make-change-gen (5 1) 13"
               (list->set (stream->list (generate (make-change-gen (list 5 1) 13))))
               (set (list 5 5 1 1 1)
                    (list 5 1 1 1 1 1 1 1 1)
                    (list 1 1 1 1 1 1 1 1 1 1 1 1 1)))

  ; A note about this test. It may seem weird that the expected output is (set)
  ; and not (set 'done). The reason is that `generate` returns `empty-stream` when
  ; there are no more choices (see its implementation above).
  (test-equal? "no combinations possible"
               (list->set (stream->list (generate (make-change-gen (list 10 3) 17))))
               (set))

  (test-equal? "larger number of combinations"
               (stream-length (generate (make-change-gen '(20 10 5 3 1) 100)))
               2787))