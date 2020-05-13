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


(say "Exercise 3.7")

(define (make-alias-account balance password)
  (let ((passwords (cons password '())))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (alias new_password)
      (set! passwords (cons password new_password)))
    (define (password-exist? lst target)
      (cond ((not (pair? lst)) (eq? lst target))
            (else
              (cond ((eq? (car lst) target) #t)
               (else (password-exist? (cdr lst) target))))))
    (define (dispatch given_password msg)
      (cond ((password-exist? passwords given_password)
        (cond ((eq? msg 'withdraw) withdraw)
              ((eq? msg 'deposit) deposit)
              ((eq? msg 'alias) alias)
              (else (error "Unknown request: MAKE-ACCOUNT" msg))))
        (else 
          (begin
            (say "Incorrect password")
            nop))))
  dispatch))

(define peter-acc (make-alias-account 100 'secret-password))
;((peter-acc 'secret-password 'withdraw) 40)
;((peter-acc 'secret-password 'alias) 'alias_password)

(define (make-joint account org_password alias_password)
  ((account org_password 'alias) alias_password)
  account)

(define paul-acc (make-joint peter-acc 'secret-password 'alias-password))

((paul-acc 'alias-password 'withdraw) 10)
((peter-acc 'secret-password 'withdraw) 20)
((paul-acc 'alias-password 'withdraw) 30)
((paul-acc 'bad-password 'withdraw) 30)

(say "Exercise 3.8")

(define (ff base)
  (define (fff val)
    (set! base (+ base val))
    base)
  fff)

(define f1 (ff -0.5))
(define f2 (ff -0.5))
 
(+ (f1 0) (f1 1))
(+ (f2 1) (f2 0))

(say "Exercise 3.9")

; recursion version
; Global Env: factorial
; Env1: n = 6
; Env2: n = 5
; Env3: n = 4
; Env4: n = 3
; Env5: n = 2
; Env6: n = 1

; iterative version
; Global Env: factorial, fact-iter
; Env1: (fact-iter 1 1 6)
; Env2: (fact-iter 6 2 5)
; Env3: (fact-iter 30 3 4)
; Env4: (fact-iter 120 4 3)
; Env5: (fact-iter 360 5 2)
; Env6: (fact-iter 720 6 1)

(say "Exercise 3.12")
; See https://stackoverflow.com/questions/9475366/set-car-set-cdr-unbound-in-racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)
; (eq? set-cdr! set-mcdr!) --> #t

(define x (mlist 'a 'b))
(define y (mlist 'c 'd))
(define z (mappend x y))
z
(mcdr x) ; x is still ('a 'b), as it's copy by value

(define (last-mpair lst)
  (cond ((eq? '() (mcdr lst))
         lst)
        (else (last-mpair (mcdr lst)))))

(define (append! x y)
  (set-mcdr! (last-mpair x) y)
  x)

(define w (append! x y))
(mcdr x) ; x is ('a 'b 'c 'd), as it's modified in place.


(say "Exercise 3.13")

(define (make-cycle x)
  (set-mcdr! (last-mpair x) x)
  x)

(define zz (make-cycle (mlist 'a 'b 'c)))
zz
;(last-mpair zz) ; it will run infinitely

(say "Exercise 3.14")

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (mcdr x)))
        (set-mcdr! x y)
        (loop temp x))))
  (loop x '()))

; it reverse the list in place
(define v (mlist 'a 'b 'c 'd)) ; a -> b -> c -> d -> null
(mystery v) ; d -> c -> b -> a -> null

(say "Exercise 3.15")
    
;        _____________
; z1 --> |  *  |  *  |
;        -------------
;           |     |
;           V     V
;        _____________     ______________
;  x --> |  *  |  *  | --> |  *  | null |
;        -------------     --------------
;           |                 |
;           V                 V
;        _______           _______
;        |'wow |           |  b  |
;        -------           -------

;        _____________     _____________     ______________
; z2 --> |  *  |  *  | --> |  *  |  *  | --> |  *  | null |
;        -------------     -------------     --------------
;            |                 |                |
;            |                 V                V
;            |              _______          _______           
;            |              |  a  |          |  b  |
;            |              -------          -------
;            |                                  ^                
;            |                                  |
;            |             _____________     ______________
;             ---------->  |  *  |  *  | --> |  *  | null |
;                          -------------     --------------
;                             |
;                             V
;                          _______
;                          |'wow |
;                          -------



(say "Exercise 3.16")

(define (count-pairs x)
  (if (not (mpair? x))
    0
    (+ (count-pairs (mcar x))
       (count-pairs (mcdr x))
       1)))

(define l3 (mlist 1 2 3))
(count-pairs l3)

(define l4 (mlist 1 2 3))
(set-mcar! l4 (mcdr (mcdr l4)))
; _____________
; |           |
; |           V
; 1 --> 2 --> 3

(count-pairs l4) ; 4 = 3 + 1

(define l7 (mlist 1 2 3))
(set-mcar! l7 (mcdr l7))
(set-mcar! (mcdr l7) (mcdr (mcdr l7)))
; _______
; |     |      
; |     V      
; 1 --> 2 --> 3
;       |     ^
;       |     |
;       -------
(count-pairs l7) ; 7 = 3 + 2 + 2

(define lcircle (mlist 1 2 3))
(set-mcar! (mcdr lcircle) lcircle)
;(count-pairs lcircle) ; this will lead to infinite loop


(say "Exercise 3.17")

(define (find-in-mlist lst target)
  (cond ((not (mpair? lst)) (eq? lst target))
        (else
          (cond ((eq? (mcar lst) target) #t)
                (else (find-in-mlist (mcdr lst) target))))))

(define (correct-count-pairs lst)
  (let ((counted '()))
    (define counted? find-in-mlist)
    (define (count-pairs x)
      (if (not (counted? counted x))
        (begin
          (set! counted (mcons x counted))
          ;(set! counted (mcons counted x)) ; do not use this! think abou why.
          (if (not (mpair? x))
            0
            (+ (count-pairs (mcar x))
               (count-pairs (mcdr x))
               1)))
        0))
    (count-pairs lst)))

(correct-count-pairs l3)
(correct-count-pairs l4)
(correct-count-pairs l7)


(say "Exercise 3.18")

(define (circle-detector lst)
  (let ((seen '()))
    (define seen? find-in-mlist)
    (define (go x)
      (if (null? x) 
        #f
        (if (seen? seen x)
          #t
          (begin
            (set! seen (mcons x seen))
            (go (mcdr x))))))
    (go lst)
    ))

(circle-detector (make-cycle (mlist 'a 'b 'c)))
(circle-detector (mlist 1 2 3))

(say "Exercise 3.19")

(define (optimized-circle-detector lst) ; runs in constant space
  (let ((head (mcar lst)))
    (find-in-mlist (mcdr lst) head)))

(optimized-circle-detector (make-cycle (mlist 'a 'b 'c)))
(optimized-circle-detector (mlist 1 2 3))

(say "Exercise 3.21")

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (mcar (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
           (else 
             (set-mcdr! (rear-ptr queue) new-pair)
             (set-rear-ptr! queue new-pair)
             queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (mcdr (front-ptr queue)))
              queue)))

(define (print-queue queue)
  (let ((front (front-ptr queue)))
    (define (print-queue-aux ptr)
      (if (mpair? ptr) 
        (begin
             (display (mcar ptr))
             (display "->")
             (print-queue-aux (mcdr ptr)))
        (say "null")))
    (print-queue-aux front)))


(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))

(say "Exercise 3.22")

(define (make-stateful-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (print-queue)
      (define (print-queue-aux ptr) 
        (if (mpair? ptr)
          (begin
            (display (mcar ptr))
            (display "->")
            (print-queue-aux (mcdr ptr)))
          (say "null")))
      (print-queue-aux front-ptr))
    (define (empty-queue?) (null? front-ptr))
    (define (insert-queue item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               (print-queue))
              (else
                (set-mcdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)
                (print-queue)))))
    (define (delete-queue)
      (cond ((empty-queue?) (error "DELETE called with empty queue"))
            (else (set! front-ptr (mcdr front-ptr))
                  (print-queue))))
    (define (dispatch msg)
      (cond ((eq? msg 'insert)
             insert-queue)
            ((eq? msg 'delete)
             delete-queue)))
    dispatch))

(define q2 (make-stateful-queue))
((q2 'insert) 'a)
((q2 'insert) 'b)
((q2 'delete))
((q2 'delete))

(say "Exercise 3.23")

(define (make-deque-node val)
  (mcons val (mcons '() '())))
(define (deque-node-val node)
  (mcar node))
(define (deque-node-front-ptr node)
  (mcar (mcdr node)))
(define (deque-node-rear-ptr node)
  (mcdr (mcdr node)))
(define (set-deque-node-cdr! node target)
  (set-mcdr! (mcdr node) target))
(define (set-deque-node-car! node target)
  (set-mcar! (mcdr node) target))
(define (deque-node-null? node)
  (and (null? deque-node-front-ptr) (null? deque-node-rear-ptr)))

(define (make-deque)
  (let ((head '())
        (tail '()))
    (define (print-deque)
      (define (print-deque-aux node) 
        (if (and (not (eq? node head)) (mpair? node))
          (begin
            (display (deque-node-val node))
            (display "->")
            (print-deque-aux (deque-node-rear-ptr node)))
          (say "null")))
      (cond ((empty-deque?) (say "null"))
            (else 
             (display (deque-node-val head)) 
             (display "->")
              (if (eq? head tail)
                (say "null")
                (print-deque-aux (deque-node-rear-ptr head))))))
    (define (empty-deque?) (null? head))
    (define (insert-deque msg item)
      (cond ((empty-deque?)
             (let ((new-node (make-deque-node item)))
               (set! head new-node)
               (set! tail new-node)))
            (else
              (let ((new-node (make-deque-node item)))
                (set-deque-node-cdr! new-node head)
                (set-deque-node-car! new-node tail)
                (set-deque-node-car! head new-node)
                (set-deque-node-cdr! tail new-node)
                (cond ((eq? msg 'front) (set! head new-node))
                      ((eq? msg 'rear) (set! tail new-node))
                      (else (error "Unsupported message" msg))))))
      (print-deque))
    (define (delete-deque msg)
      (cond ((empty-deque?) (error "DELETE called with empty queue"))
            ((eq? (deque-node-rear-ptr head) head) ; when only one node left
              (set! head '()) 
              (set! tail '()))
            ((eq? msg 'front)
             (set-deque-node-cdr! tail (deque-node-rear-ptr head))
             (set-deque-node-car! (deque-node-rear-ptr head) tail)
             (set! head (deque-node-rear-ptr tail)))
            ((eq? msg 'rear)
             (set-deque-node-car! head (deque-node-front-ptr tail))
             (set-deque-node-cdr! (deque-node-front-ptr tail) head)
             (set! tail (deque-node-front-ptr head)))
            (else (error "Unsupported message" msg)))
      (print-deque))
    (define (dispatch msg)
      (cond ((eq? msg 'insert)
             insert-deque)
            ((eq? msg 'delete)
             delete-deque)
            ))
    dispatch))

(define dq (make-deque))
((dq 'insert) 'front 'a)
((dq 'insert) 'front 'b)
((dq 'insert) 'rear 'c)
((dq 'insert) 'front 'd)
((dq 'delete) 'front)
((dq 'delete) 'rear)
((dq 'delete) 'front)
((dq 'delete) 'rear)

(say "Exercise 3.24")

(define (mcaar pair)
  (mcar (mcar pair)))

(define (make-1d-table same-key?) 
  (let ((table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcaar records)) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup key)
      (let ((record (assoc key (mcdr table))))
        (if record 
          (mcdr record)
          false)))
    (define (insert! key value)
      (let ((record (assoc key (mcdr table))))
        (if record
          (set-mcdr! record value)
          (set-mcdr! table
                    (mcons (mcons key value)
                           (mcdr table)))))
      'ok)
    (define (dispatch msg)
      (cond ((eq? msg 'insert!)
             insert!)
            ((eq? msg 'lookup)
             lookup)
            (else (error "Unsupported msg" msg))))
    dispatch))

(define table1 (make-1d-table
  (λ (x y) 
     (if (< (abs (- x y)) 5) #t #f))))

((table1 'insert!) 1 'a)
((table1 'insert!) 10 'b)
((table1 'insert!) 100 'c)
((table1 'lookup) 1)
((table1 'lookup) 6)


(say "Exercise 3.25")


(define (make-table) 
  (let ((table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((eq? key (mcaar records)) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup keys)
      (define (find-subtable lst subtable) ; find subtable, if not exist, return #f
        (define key (if (mpair? lst) (mcar lst) lst))
        (let ((record
                (assoc
                  key
                  (mcdr subtable))))
          (if record
            (if (null? (mcdr lst)) ; it's alreay the last element
              (mcdr record) ; return what has been found
              (find-subtable (mcdr lst) record)) ; dig deeper
            #f)))
      (find-subtable keys table))

    (define (insert! keys value)
      (define (find-subtable lst subtable) ; find subtable, if not exist, build it
        (define key (if (mpair? lst) (mcar lst) lst))
        (if (not (mpair? (mcdr lst))) ; if it's last element
          subtable
          (let ((record 
                  (assoc 
                    key
                    (mcdr subtable))))
            (if record
              (find-subtable (mcdr lst) record) ; dig deeper
              (begin ; build table
               (set-mcdr! subtable (mcons (mcons key '()) (mcdr subtable)))
               (find-subtable (mcdr lst) (mcar (mcdr subtable)))
               ))
            )))
      (let ((subtable (find-subtable keys table))
            (last-key (mcar (last-mpair keys))))
        (let ((record (assoc last-key (mcdr subtable))))
          (if record
            (set-mcdr! record value)
            (set-mcdr! subtable
                       (mcons (mcons last-key value)
                              (mcdr subtable))))))
      'ok 
      )
    (define (dispatch msg)
      (cond ((eq? msg 'insert!)
             insert!)
            ((eq? msg 'lookup)
             lookup)
            (else (error "Unsupported msg" msg))))
    dispatch))

(define table (make-table))
((table 'insert!) (mlist 1 2 3) 'a)
((table 'insert!) (mlist 1 2 4) 'b)
((table 'lookup) (mlist 1 2 3))
((table 'lookup) (mlist 1 2 4))
((table 'lookup) (mlist 1 2 5))
((table 'lookup) (mlist 1 2))


