#lang racket

(define (say sth)
  (begin
    (display sth)
    (display "\n")
  )
)

(say "Exercise 3.1")

(define (make-accumulator base)
  (define (add inc)
    (begin
      (set! base (+ base inc))
      base
    )
  )
  add
)

(define A (make-accumulator 5))

(A 10)
(A 10)


(say "Exercise 3.2")

(define (make-monitored f)
  (let ((counter 0))
    (define (call arg)
      (begin
        (set! counter (+ counter 1))
        (f arg)))
    (define (reset-counter)
      (set! counter 0))
    (define (show)
      (say counter))
    (define (dispatch msg)
      (cond ((eq? msg 'how-many-calls?) (show))
            ((eq? msg 'reset-count) (reset-counter))
            (else (call msg))))
    dispatch))

(define s1 (make-monitored sqrt))
(define s2 (make-monitored sqrt))
(s1 100)
(s1 'how-many-calls?)
(s2 'how-many-calls?)

(say "Exercise 3.3")

(define (nop arg)
  (display "")
  )

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd msg)
    (cond ((eq? pwd password)
      (cond ((eq? msg 'withdraw) withdraw)
            ((eq? msg 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT" msg))))
      (else 
        (begin
          (say "Incorrect password")
          nop))))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'bad-password 'deposit) 50)


(say "Exercise 3.4")

(define (make-secure-account balance password)
  (let ((tried 0))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (reset-tried)
      (set! tried 0))
    (define (inc-tried)
      (set! tried (+ tried 1)))
    (define (call-the-cops)
      (say "Calling the cops..."))
    (define (dispatch pwd msg)
      (cond ((< tried 7)
        (cond ((eq? pwd password)
          (begin
            (reset-tried)
            (cond ((eq? msg 'withdraw) withdraw)
                  ((eq? msg 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUNT" msg)))))
              (else (begin
                      (inc-tried)
                      (display "Incorrect password for ")
                      (display tried)
                      (say " consequtive times")
                      nop))))
            (else (begin
                    (call-the-cops)
                    nop))))
  dispatch))

(define sacc (make-secure-account 100 'secret-password))
((sacc 'secret-password 'withdraw) 40)
(for ([i 8])
  ((sacc 'bad-password 'deposit) 10))


(say "Exercise 3.5")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

;(random-in-range 5.0 10.0)
;(random-in-range 10.0 5.0)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(define (square x)
  (* x x))

(define (calc-dist x1 y1 x2 y2)
  (sqrt (+ (square (- x1 x2)) (square (- y1 y2)))))

;(calc-dist 0 0 1 1)

(define (calc-area x1 y1 x2 y2) ; take the diagonal
  (* (abs (- x1 x2)) (abs (- y1 y2))))

;(calc-area 0 0 2 2)

(define (make-experiment x1 y1 x2 y2 midx midy)
  (define (estimate-integral-experiment)
    (let ((px (random-in-range x1 x2)) (py (random-in-range y1 y2)))
      (cond ((<= (calc-dist px py midx midy) 1)
             #t)
            (else #f))))
  estimate-integral-experiment)

;((make-experiment 0 0 2 2 1 1))

(define (estimate-integral x1 y1 x2 y2 trials)
  (let ((experiment (make-experiment x1 y1 x2 y2 (/ (abs (+ x1 x2)) 2) (/ (abs (+ y1 y2)) 2))))
    (exact->inexact 
      (* (monte-carlo trials experiment) (calc-area x1 y1 x2 y2)))))

(estimate-integral 0 0 2 2 100000)
;(estimate-integral 2 4 8 10 100000)


(say "Exercise 3.6")

(define (pseudo-random)
  (let ((seed 42))
    (define (set-seed val)
      (set! seed val)
      seed
      )
    (define (next)
      (let ((val (modulo (+ (* 12345 seed) 67890) 2147483647)))
        (set! seed val)
        val
      ))
    (define (dispatch cmd)
      (cond ((eq? cmd 'generate)
             (next))
            ((eq? cmd 'reset)
             set-seed)
            (else (error "Wrong command"))))
    dispatch))

(define generator (pseudo-random))
(generator 'generate)
(generator 'generate)
((generator 'reset) 42)
(generator 'generate)
(generator 'generate)

