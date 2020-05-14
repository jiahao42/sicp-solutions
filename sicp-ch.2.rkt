#lang racket

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
         
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< n 0)
        (cons (/ (- 0 n) g) (/ (- 0 d) g))
        (cons (/ n g) (/ d g)))))
          

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (div-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;; Exercise 2.1
(display "\nExercise 2.1")

(define minus-one-third (make-rat 1 -3))
(print-rat minus-one-third)
(define minus-one-minus-third (make-rat -1 -3))
(print-rat minus-one-minus-third)
(define one-minus-third (make-rat -1 3))
(print-rat one-minus-third)



;; Exercise 2.2
(display "\nExercise 2.2\n")

(define (make-point x y)
  (cons x y))

(define start-segment car)
(define end-segment cdr)
(define x-point car)
(define y-point cdr)

(define (make-segment start end)
  (cons start end))

(define sample-point (make-point 1 2))
(define sample-segment (make-segment (make-point 1 2) (make-point 3 4)))


(define (print-point p)
  ; (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point sample-point)

(define (sum-squares a b)
  (if (> a b)
    0
    (+ (* a a)
      (sum-squares (+ a 1) b))))

;; Exercise 2.4
(display "\nExercise 2.4\n")

(define (_cons x y)
  (lambda (m) (m x y)))
(define (_car z)
  (z (lambda (p q) p)))

(define (_cdr z)
  (z (lambda (p q) q)))

(_car (_cons 1 2))
(_cdr (_cons 2 3))



;; Exercise 2.6
(display "\nExercise 2.6\n")

(define zero (lambda (a) (lambda (b) b)))
(define (add-1 n)
  (lambda (a) (lambda (b) (a ((n a) b)))))

zero

(define (inc n)
  (+ n 1))
((zero inc) 0)
((zero inc) 1)
((zero inc) 2)

(define one (add-1 zero))
((one inc) 0)
((one inc) 1)
((one inc) 2)
(define two (add-1 one))
((two inc) 0)
((two inc) 1)
((two inc) 2)

(define my-one (lambda (a) (lambda (b) (a b))))
((my-one inc) 0)
(define my-two (lambda (a) (lambda (b) (a (a b)))))
((my-two inc) 0)


(define (add-church m n)
   (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(define three (add-church one two))
(define four (add-church two two))
(define seven (add-church three four))

((three inc) 0)
((four inc) 0)
((seven inc) 0)

;; Extended Exercise
(display "\nExtended Exercise\n")

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval-1 x y)
  (mul-interval
   x
   (let ((upper-reciprocal (/ 1.0 (upper-bound y)))
         (lower-reciprocal (/ 1.0 (lower-bound y))))
     (if (> (* upper-reciprocal lower-reciprocal) 0)
         (make-interval upper-reciprocal lower-reciprocal)
         (make-interval lower-reciprocal upper-reciprocal)))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval
    (/ 1.0 (upper-bound y))
    (/ 1.0 (lower-bound y)))))
                  

(define (make-interval a b) (cons a b))

;; Exercise 2.7
(display "\nExercise 2.7\n")

(define lower-bound car)
(define upper-bound cdr)

(define (print-interval x)
  ;; (newline)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]"))


;; [3, 5]
(define sample-interval-1 (add-interval (make-interval 1 2) (make-interval 2 3)))
;; [7, 9]
(define sample-interval-2 (add-interval (make-interval 3 4) (make-interval 4 5)))

(lower-bound sample-interval-1)
(upper-bound sample-interval-1)

(print-interval sample-interval-1)
(print-interval sample-interval-2)


;; Exercise 2.8
(display "\nExercise 2.8\n")

(define (sub-interval x y)
   (let ((p1 (abs (- (upper-bound y) (upper-bound x))))
         (p2 (abs (- (upper-bound y) (lower-bound x))))
         (p3 (abs (- (lower-bound y) (upper-bound x))))
         (p4 (abs (- (lower-bound y) (lower-bound x)))))
     (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(print-interval (sub-interval sample-interval-2 sample-interval-1))
         

;; Exercise 2.10
(display "\nExercise 2.10\n")

;; [-5, 1]
(define sample-interval-3 (add-interval (make-interval -2 -1) (make-interval -3 2)))


(print-interval sample-interval-3)
(print-interval (add-interval sample-interval-2 sample-interval-1))
(print-interval (div-interval sample-interval-1 sample-interval-3))
(print-interval (div-interval sample-interval-3 sample-interval-1))
(print-interval (div-interval sample-interval-3 sample-interval-3))


;; 2.2 Hierarchical Data and the Closure Property
(display "\n2.2 Hierarchical Data and the Closure Property\n")

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append squares odds)


;; Exercise 2.17
(display "\nExercise 2.17\n")

(define (last-pair l)
  (if (equal? l '())
      '()
      (if (equal? (cdr l) '())
          (car l)
          (last-pair (cdr l)))))

(last-pair (list 1 2 3 4))
(last-pair '())

;; Exercise 2.18
(display "\nExercise 2.18\n")

(define (reverse l)
 (if (null? l)
     l
     (append (reverse (cdr l)) (list (car l)))))

(reverse (list 1 2 3 4 5))


(cons 1 (cons 2 (cons 3 '())))

;; Exercise 2.20
(display "\nExercise 2.20\n")

(define (same-parity x . z)
  (define (filter l type)
    (if (null? l)
        " Done!"
        (if (= type 0) ;; type even
            (if (= (modulo (car l) 2) 0) ;; if even
                (begin
                  (display (car l))
                  (filter (cdr l) type))
                (filter (cdr l) type))
            (if (= (modulo (car l) 2) 0) ;; type odd, if even
                (filter (cdr l) type)
                (begin
                  (display (car l))
                  (filter (cdr l) type))))))
  (if (= (modulo x 2) 0) ;; even
      (filter z 0)
      (filter z 1)))

(same-parity 1 2 3 4 5 6 7 8 9)
      
(define (say msg) (display msg) (display "\n"))

(say "Exercise 2.21")
(define (square x) (* x x))
(define (square-list1 items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list1 (cdr items)))))
(square-list1 (list 1 2 3 4))

(define (square-list2 items)
  (map square items))
(square-list2 (list 1 2 3 4))


(say "Execise 2.22")

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer (square (car things)))))) ; won't work because '() should be consed to the right side.
                  ;answer))))
  (iter items '()))
(square-list-iter (list 1 2 3 4))



