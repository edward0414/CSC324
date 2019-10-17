#lang racket #| â˜… CSC324 Fall 2019: Exercise 5 â˜… |#
#|
Module: ex5
Description: Exercise 5: Extending the my-class Macro
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019

Before starting, please review the exercise guidelines at
<https://www.cs.toronto.edu/~david/csc324/homework.html>.
|#
;-------------------------------------------------------------------------------
(provide my-class-getter my-class-setter)


; Returns a thunk that raises an attribute error (with an appropriate message).
(define (attribute-error object attr)
  (thunk (error (format "~a has no attribute ~a." object attr))))

; From a previous exercise ;)
(define (fix-first x f)
  (lambda args (apply f (cons x args))))


;-------------------------------------------------------------------------------
; â˜… Task 1: Accessor functions â˜…
;-------------------------------------------------------------------------------
#|
(my-class-getter <class-name> (<attr> ...)
  (method (<method-name> <param> ...) <body>) ...)

  This macro accepts the *exact* same pattern as my-class from lecture,
  and defines a constructor function that behaves in the same way as well.

  In addition to defining the constructor, my-class-getter defines
  *one new accessor function per attribute of the class*, using the name
  of the attribute as the name of the function.
|#
(define-syntax my-class-getter
  (syntax-rules (method)
    [(my-class-getter <class-name> (<attr> ...)
                      (method (<method-name> <param> ...) <body>) ...)

     ; Template (the code we want to run)
     (begin
       ; This is the constructor from lecture.
       ; You may change this, but it is possible to complete this task without
       ; touching this `define` at all.
       (define class__dict__
         ; Dictionary of methods. Shared among all instances.
         (make-immutable-hash
          (list (cons (quote <method-name>)
                      (lambda (<param> ...) <body>)) ...)))
       (define (<class-name> <attr> ...)
         (letrec ([self__dict__
                   ; Dicionary of instance attributes.
                   (make-immutable-hash
                    (list (cons (quote <attr>) <attr>) ...))]
                  [me (lambda (msg)
                        (cond
                          ; Note the chained lookup here! First self__dict__, then class__dict__
                          [(hash-has-key? self__dict__ msg)
                           (hash-ref self__dict__ msg)]
                          [(hash-has-key? class__dict__ msg)
                           (fix-first me (hash-ref class__dict__ msg))]
                          [else ((attribute-error (quote <class-name>) msg))]))])
           me))

       ; You can (and should) add more definitions here.
       (define (<attr> <class-name>)
         (<class-name> (quote <attr>)))...
                                       )]))


(module+ test
  (require rackunit)

  ; We use `local` to create a local scope for our definitions.
  ; Run these tests when you're ready!
  (local
    [(my-class-getter Point (x y)
                      (method (size self)
                              (sqrt (+ (* (self 'x) (self 'x)) (* (self 'y) (self 'y)))))

                      (method (scale self n)
                              (Point (* (self 'x) n) (* (self 'y) n))))]
    (test-true "x and y are functions" (and (procedure? x) (procedure? y)))
    (test-equal? "x and y are accessors"
                 (let* ([p (Point 2 3)])
                   (list (x p) (y p)))
                 (list 2 3))))


;-------------------------------------------------------------------------------
; â˜… Task 2: Simulating mutation â˜…
;-------------------------------------------------------------------------------
#|
(my-class-setter <class-name> (<attr> ...)
  (method (<method-name> <param> ...) <body>) ...)

  This macro accepts the *exact* same pattern as my-class from lecture,
  and defines a constructor function that behaves in the same way as well.

  The object returned by the constructor behaves the exact same as the corresponding
  object created by using `my-class`, with one exception. When sent the message
  '__setattr__, the object responds with a method that takes in a symbol <attr> and an
  expression <expr>, and returns a new object that behaves exactly the same as the original,
  except when sent the message <id>, it returns the value of <expr>.
  This is true whether <attr> is an existing attribute of the original object or not.

  Of course, because you may not use mutation, the original object must remain unchanged.

  Implementation hint:
    The main challenge of this task is that the existing my-class macro puts the creation of the
    __dict__ hash tables and the creation of the object ("me") all in one function. This makes
    it hard to modify just one of the __dict__ mappings.

    So the easiest way to complete this task is to FIRST refactor the existing code to pull out
    the creation of the object "me" itself into a separate helper. See below for the helper
    we've started for you (you're free to change/ignore it, but we advise using it).
|#
(define-syntax my-class-setter
  (syntax-rules (method)
    [(my-class-setter <class-name> (<attr> ...)
                      (method (<method-name> <param> ...) <body>) ...)

     (begin
       (define class__dict__
         ; Dictionary of methods. Shared among all instances.
         (make-immutable-hash
          (list (cons (quote <method-name>)
                      (lambda (<param> ...) <body>)) ...)))
       (define (<class-name> <attr> ...)
         (letrec ([self__dict__
                   ; Dicionary of instance attributes.
                   (make-immutable-hash
                    (list (cons (quote <attr>) <attr>) ...))]
                  [me (lambda (msg)
                        (cond
                          ; Note the chained lookup here! First self__dict__, then class__dict__
                          [(hash-has-key? self__dict__ msg)
                           (hash-ref self__dict__ msg)]
                          [(hash-has-key? class__dict__ msg)
                           (fix-first me (hash-ref class__dict__ msg))]
                          [else ((attribute-error (quote <class-name>) msg))]))])
           me)))
     ]))


#|
(create-object class-name class__dict__ self__dict__)
  class-name: symbol?
    The name of the class of this object.
  class__dict__: hash?
    The class dictionary (used for method lookup).
  self__dict__: hash?
    The instance dictionary (used for other instance attributes).
  
  Returns an object (in the same representation we use in CSC324) that uses
  the given dictionaries to define its attributes.
  You may or may not want to add '__setattr__ here as well.
|#
(define (create-object class-name class__dict__ self__dict__)
  (void))


#;(module+ test
    (local
      [(my-class-setter Point (x y)
                        (method (size self)
                                (sqrt (+ (* (self 'x) (self 'x))
                                         (* (self 'y) (self 'y)))))

                        (method (scale self n)
                                (Point (* (self 'x) n) (* (self 'y) n))))]
      (test-true "__setattr__ is a method" (procedure? ((Point 2 3) '__setattr__)))
      (test-equal? "__setattr__ changes an attribute"
                   (let* ([p (Point 2 3)]
                          [p2 ((p '__setattr__) 'x 5)])
                     (p2 'x))
                   5)
      (test-equal? "__setattr__ adds a new attribute"
                   (let* ([p (Point 2 3)]
                          [p2 ((p '__setattr__) 'z 5)])
                     (p2 'z))
                   5)
      (test-equal? "__setattr__ doesn't mutate original object"
                   (let* ([p (Point 2 3)]
                          [p2 ((p '__setattr__) 'x 5)])
                     (p 'x))
                   2)
      (test-equal? "Multiple __setattr__ chained"
                   (let* ([p (Point 2 3)]
                          [p2 ((p '__setattr__) 'x 5)]
                          [p3 ((p2 '__setattr__) 'x 10)])
                     (p3 'x))
                   10)))