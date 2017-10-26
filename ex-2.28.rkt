#lang racket

; (define (fringe x)
;   (define (helper x l)
;     (cond ((null? x) l)
;           ((integer? x) (cons x l))
;           ((list? x) (helper (cadr x) (helper (car x) l))) 
;           (else (helper (cdr x) (helper (car x) l)))))
;   (reverse (helper x '())))


; Ref: http://community.schemewiki.org/?sicp-ex-2.28
; By zhenhuaa
(define (fringe x) 
  (cond ((null? x) x) 
        ((number? x) (list x)) 
        (else (append (fringe (car x)) 
                      (fringe (cdr x)))))) 

(define x (list (list 1 2) (list 3 4)))

(fringe x)
;'(1 2 3 4)

(fringe (list x x))
;'(1 2 3 4 1 2 3 4)
