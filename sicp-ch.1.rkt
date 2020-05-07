#lang racket


;; Exercise 1.6
(display "Exercise 1.6\n")
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (test)
  (display "test"))

;(test)

; (new-if (= 2 2) test test)
; (if (= 2 2) test test)

(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; (sqrt 931233324324234)

;; Exercise 1.7
(display "Exercise 1.7\n")

(define (better-sqrt-iter old-guess new-guess x)
  (if (better-good-enough? old-guess new-guess)
      new-guess
      (better-sqrt-iter new-guess (improve new-guess x) x)))

(define (better-good-enough? old-guess new-guess)
  (< (abs (- old-guess new-guess)) 0.001))

(define (better-sqrt x)
  (better-sqrt-iter 0 1.0 x))

(define mm 93125222452222)
; (better-sqrt mm)
; (sqrt mm)


;; Exercise 1.8
(display "Exercise 1.8\n")

(define (improve-cube guess x)
  (average guess (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)))

(define (cube-iter old-guess new-guess x)
  (if (better-good-enough? old-guess new-guess)
      new-guess
      (cube-iter new-guess (improve-cube new-guess x) x)))


(define (cube-root x)
  (cube-iter 0 1.0 x))


(cube-root 8)

;; Exercise 1.10
(display "Exercise 1.10\n")

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))


(A 1 10)
(A 2 4)
(A 3 3)

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b n)
  (if (= n 0)
      b
      (fib-iter (+ a b) a (- n 1))))

(fib 1)
(fib 0)
(fib 5)
(fib 7)


(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 11)


;; Exercise 1.11
(display "Exercise 1.11\n")

(define (f11 n)
  (if (< n 3)
      n
      (+ (f11 (- n 1)) (* 2 (f11 (- n 2))) (* 3 (f11 (- n 3))))))

(f11 1)
(f11 5)

(define (f11-iter n)
  (if (< n 3)
      n
      (f11-iter-aux 0 1 2 n)))

(define (f11-iter-aux min mid max n)
  (if (< n 3)
      max
      (f11-iter-aux mid max (+ max (* 2 mid) (* 3 min)) (- n 1))))

(f11-iter 1)
(f11-iter 5)

;; Exercise 1.12
(display "Exercise 1.12\n")

(define (pascal col row)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal (- col 1) (- row 1)) (pascal col (- row 1)))))

(pascal 3 5)
  
;; Exercise 1.16
(display "Exercise 1.16\n")

(define (fast-expt-iter b n)
  (fast-expt-iter-aux 1 b n))

(define (fast-expt-iter-aux a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter-aux a (square b) (/ n 2)))
        (else (fast-expt-iter-aux (* a b) b (- n 1)))))

(fast-expt-iter 2 4)
(fast-expt-iter 3 3)


;; Exercise 1.21
(display "Exercise 1.21\n")

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divide? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
(smallest-divisor 100)

(prime? 100)
(prime? 123)


;; Exercise 1.22
(display "Exercise 1.22\n")

(define (runtime) (current-milliseconds))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time)) (display " not found")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 19999)
(timed-prime-test 99999321431243214123411)
(timed-prime-test 10009990000)

(define (search-for-primes i n)
  (define (search-for-primes-aux ii nn)
    (if (prime? ii)
        (begin (display ii)
         (newline)
         (if (< nn n)
             (search-for-primes-aux (+ 1 ii) (+ 1 nn))
             (display "end!\n")))
        (search-for-primes-aux (+ 1 ii) nn)))
  (search-for-primes-aux i 0))

(search-for-primes 100 5)
(search-for-primes 10000 5)


;; Exercise 1.23
(display "Exercise 1.23\n")

(define (better-prime? n)
  (= n (better-smallest-divisor n)))
(define (better-smallest-divisor n) (better-find-divisor n 2))
(define (better-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? test-divisor n) test-divisor)
        (else (better-find-divisor n
                            (if (= test-divisor 2) 3 (+ test-divisor 2))))))
(better-prime? 100000)
(better-prime? 5)


(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(sum-integers 3 5)


;; Exercise 1.30
(display "Exercise 1.30\n")

(define (custom-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc n)
  (+ 1 n))
(define (cube n)
  (* n n n))

(custom-sum cube 0 inc 10)


;; Exercise 1.31
(display "Exercise 1.31\n")

(define (custom-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (dummy n)
  n)
(custom-product dummy 1 inc 10)

(define (pi-product a b)
  (if (= a 2)
      (/ 2 3)
      (if (< a b)
          (/ (+ a 2) b)
          (/ a (+ b 2)))))

; (custom-product dummy 1 pi-product 1000)


;; Exercise 1.32a
(display "Exercise 1.32a\n")

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter a null-value)
    (if (> a b)
        null-value
        (accumulate-iter (next a) (combiner (term a) null-value))))
  (accumulate-iter a null-value))


(define (accumulate-product a b)
  (accumulate * 1 dummy a inc b))

(accumulate-product 1 5)

(define (accumulate-sum a b)
  (accumulate + 0 dummy a inc b))

(accumulate-sum 1 10)

;; Exercise 1.32b
(display "Exercise 1.32b\n")

(define (accumulate-re combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-re combiner (combiner (term a) null-value) term (next a) next b)))

(define (accumulate-re-product a b)
  (accumulate-re * 1 dummy a inc b))

(accumulate-re-product 1 5)

(define (accumulate-re-sum a b)
  (accumulate-re + 0 dummy a inc b))

(accumulate-re-sum 1 10)

;; Exercise 1.33
(display "Exercise 1.33\n")

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(f 1 2)


(define x 3)
(+ (let ((x 3))
     (+ x (* x 10)))
   x)


;; Exercise 1.34
(display "Exercise 1.34\n")

;; (define (f34 g) (g 2))

;; (f34 f34)


;; fixedx point: f(x) = x

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;; Newton's method
;; Dg(x) = (g(x + dx) - g(x)) / dx

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(cube 5)
((deriv cube) 5)


(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (newton-sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; Exercise 1.40
(display "Exercise 1.40\n")

(define (cubic a b c)
  (lambda (x)
    (+
     (* x x x)
     (* a x x)
     (* b x)
     c)))

(newtons-method (cubic 1 2 3) 1)

;; Exercise 1.41
(display "Exercise 1.41\n")

(define (double f)
  (lambda (x)
    (begin
      (f (f x)))))

((double inc) 5)

(((double (double double)) inc) 5)


;; Exercise 1.42
(display "Exercise 1.42\n")

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)


;; Exercise 1.43
(display "Exercise 1.43\n")

(define (repeated f n)
  (if (= 1 n)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)



;; Exercise 1.44
(display "Exercise 1.44\n")

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smooth n)
  (repeated smooth n))

((smooth sqrt) 5)
(((n-smooth 5) sqrt) 5)


;; Exercise 1.45
(display "Exercise 1.45\n")


