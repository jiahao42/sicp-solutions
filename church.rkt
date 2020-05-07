#lang racket

(define zero
  (lambda (f)
    (lambda (x) x)))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(zero 3)
((zero 3) 1)

((lambda (x y) (+ x y)) 1 2)

(define fk
  (lambda (x) (* x x)))

; (fk 100)

((zero fk) 2)
((one fk) 2)
((two fk) 2)