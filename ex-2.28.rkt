#lang racket

(define (fringe x)
  (define (helper x l)
    (cond ((null? x) l)
          ((integer? x) (cons x l))
          ((list? x) (helper (cadr x) (helper (car x) l))) 
          (else (helper (cdr x) (helper (car x) l)))))
  (reverse (helper x '())))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
;'(1 2 3 4)

(fringe (list x x))
;'(1 2 3 4 1 2 3 4)
