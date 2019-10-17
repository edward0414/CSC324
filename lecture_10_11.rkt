#lang racket #| â˜… CSC324 Fall 2019: Week 5 Example Code â˜… |#

; Returns a thunk that raises an attribute error (with an appropriate message).
(define (attribute-error object attr)
  (thunk (error (format "~a has no attribute ~a." object attr))))

; From a previous exercise ;)
(define (fix-first x f)
  (lambda args (apply f (cons x args))))


(define-syntax my-class
  (syntax-rules (method)
    [; Pattern (the code we want to write)
     (my-class <class-name> (<attr> ...)
               (method (<method-name> <param> ...)
                       <body>) ...)

     ; Template (the code we want to run)
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
           me)))]))


; Uses of the my-class macro.
(my-class Point (x y)
          (method (size self)
                  (sqrt (+ (* (self 'x) (self 'x))
                           (* (self 'y) (self 'y)))))

          (method (same-radius self other)
                  (equal? ((self 'size)) ((other 'size)))))




(let* ([p (Point 2 3)])
  ((p'size)))