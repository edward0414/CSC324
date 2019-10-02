#lang racket #| â˜… CSC324 Fall 2019: Exercise 4 â˜… |#
#|
Module: ex4
Description: Exercise 4: Strictness Analysis
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019

Before starting, please review the exercise guidelines at
<https://www.cs.toronto.edu/~david/csc324/homework.html>.
|#
;-------------------------------------------------------------------------------
(provide analyze-strictness)


;-------------------------------------------------------------------------------
; â˜… Task 1: Strictness analysis â˜…
;-------------------------------------------------------------------------------
#|
(analyze-strictness func-defs) -> (hash/c symbol? (listof integer?))
  func-defs: datum?
      A datum representing a program as specified by the grammar on the handout.
      (In this case, it's a list of function definitions.)

  Returns a *strictness map*, which is a hash table mapping function names (as symbols)
  to a list of indexes of the strict parameters of that function.
  Each list should be in increasing order.

  Remember, you may assume that the program has no syntactic or semantic errors
  (so you don't even need to watch out for things like duplicate names).

  Hash table reference: https://docs.racket-lang.org/reference/hashtables.html.

  Implementation hints:
    1. Same as Exercise 3, the main work can be done by processing the list of
       function definitions in a call to foldl.
    2. Working with list indexes is a bit more annoying in pure functional world.
       Use the list function `indexes-where`, which is similar to `filter` except
       it returns indexes rather than elements.
|#



#| contains helper, if a val exists in a list |#
(define (contains lst val)
  (cond ((member val lst) '#t) (else '#f)))

#| list helper, creating a list of strict args |#
(define (strictness-herlper args body)
  (cond
    [(list? body) (indexes-where args (lambda (val) (contains body val)))]
    [else (indexes-of args body)]))

#| foldl helper, creating strictness map for each expr |#
#| Expr: index 0 is 'define, index 1 is args, index 2 is body |#
(define (analyze-strictness-helper expr hash)
  (let ([args-with-func (second expr)])
    (hash-set hash (first args-with-func) (strictness-herlper (rest args-with-func) (third expr)))))

(define (analyze-strictness func-defs)
  (foldl analyze-strictness-helper (hash) func-defs))

#|
(strict-in? s-map id expr) -> boolean?
  s-map: (hash/c symbol? (listof integer?))
    A strictness map
  id: symbol?
    An identifier
  expr: datum?
    A datum representing a single expression (<expr> in the grammar).

  Returns whether `id` is strict in the given expression.
  Uses the given strictness map to determine strictness in function calls.

  NOTE: this function isn't being tested explicitly, so you may freely change it or
  ignore it for this exercise.

  Implementation hint:
    Remember that `and` and `or` aren't function identifiers, you can't pass them to
    HOFs like `map` or `apply`. But you can use functions like `andmap` and `ormap`
    to achieve similar effects.
|#
(define (strict-in? s-map id expr)
  (void))


(module+ test
    (require rackunit)

    (test-equal? "Identity function"
                 (analyze-strictness '((define (f x) x)))
                 (hash
                  'f (list 0)))

    (test-equal? "One function, with +"
                 (analyze-strictness '((define (f x y z) (+ x y))))
                 (hash
                  'f (list 0 1)))

    (test-equal? "Two functions, first all strict"
                 (analyze-strictness '((define (f1 x y z) (+ x y z))
                                       (define (f2 a b) (f1 b 5 10))))
                 (hash
                  'f1 (list 0 1 2)
                  'f2 (list 1))))