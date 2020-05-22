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

(say "Exercise 2.23")

(define (my-for-each func lst)
  (if (null? lst)
    #t
    (begin
      (func (car lst))
      (my-for-each func (cdr lst)))))
(my-for-each (λ (x) (say (square x))) (list 1 2 3 4))


(say "Exercise 2.24")

(list 1 (list 2 (list 3 4)))
; (1 (2 (3 4)))
;     / \
;    1  (2 (3 4))
;       / \
;      2  (3 4)
;          / \
;         3   4
;

(say "Exercise 2.25")
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(cadr (cadr (cadr (cadr (cadr (cadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))

(say "Exercise 2.26")
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; '(1 2 3 4 5 6)
(cons x y) ; '((1 2 3) 4 5 6)
(list x y) ; '((1 2 3) (4 5 6))

(say "Exercise 2.27")

(define (deep-reverse l)
 (if (null? l)
     l
     (append (deep-reverse 
               (cdr l)) 
             (cons (reverse (car l)) '()))))
(reverse (list (list 1 2) (list 3 4)))
(deep-reverse (list (list 1 2) (list 3 4)))

(say "Exercise 2.28")
(define (fringe root)
  (if (null? root)
    '()
    (if (pair? root)
      (append (fringe (car root)) (fringe (cdr root)))
      (list root))))
(define tree1 (cons (list 1 2) (list 3 4)))
(define tree2 (cons (list 1 2) (list 3 4 (list 5 6))))
tree1
(fringe tree1)
tree2
(fringe tree2)

(say "Exercise 2.29")
;; 2.29.a
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
;      mobile
;     /     \
;   branch  branch
;
;      branch
;     /      \
;   length   weight/mobile
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length mobile)
  (left-branch mobile))
(define (branch-structure mobile)
  (right-branch mobile))
;; 2.29.b
(say "Exercise 2.29.b")
(define (total-weight mobile)
  (if 
    (not (pair? mobile)) ; weight
      mobile
      (+ 
        (if (pair? (car mobile))
          (total-weight (car mobile)) ; mobile
          0) ; length
        (total-weight (cadr mobile)))))
(define branch-1 (make-branch 5 10))
(define sub-mobile (make-mobile branch-1 branch-1))
(define branch-2 (make-branch 5 sub-mobile))
(define mobile (make-mobile branch-1 branch-2))
(total-weight mobile)
(total-weight branch-1)
(total-weight branch-2)
;; 2.29.c
(say "Exercise 2.29.c")
(define (total-product mobile)
  (if (not (pair? mobile)) ; weight
    mobile
    (if (pair? (car mobile)) ; left side
      (+ 
        (total-product (car mobile)) ; mobile
        (total-product (cadr mobile)))
      (+
        0
        (* (car mobile) (total-product (cadr mobile)))))))
(total-product mobile)
(total-product branch-1)
(total-product branch-2)
(total-product sub-mobile)
(define (mobile? mobile)
  (and (pair? mobile) (pair? (car mobile)) (pair? (cadr mobile))))
(define (balanced? mobile)
  (if (null? mobile)
    #t
    (if (mobile? mobile)
      (eq? (total-product (car mobile)) (total-product (cadr mobile)))
      (and (balanced? (cadr mobile)) (balanced? (car mobile))))))
(balanced? mobile)
(balanced? (make-mobile sub-mobile sub-mobile))

; 2.29.d
(say "Exercise 2.29.d")
; The key is to change "cadr" to "cdr"
; as there is no '() attached at the end of list 
(define (make-mobile-alt left right)
  (cons left right))
(define (make-branch-alt length structure)
  (cons length structure))
(define branch-1-alt (make-branch-alt 5 10))
(define sub-mobile-alt (make-mobile-alt branch-1-alt branch-1-alt))
(define branch-2-alt (make-branch-alt 5 sub-mobile-alt))
(define mobile-alt (make-mobile-alt branch-1-alt branch-2-alt))
(define (total-weight-alt mobile)
  (if (not (pair? mobile)) ; weight
    mobile
    (+ 
      (if (pair? (car mobile))
        (total-weight-alt (car mobile)) ; mobile
        0) ; length
      (total-weight-alt (cdr mobile)))))
(total-weight-alt mobile-alt)
(total-weight-alt branch-1-alt)
(total-weight-alt branch-2-alt)
(define (total-product-alt mobile)
  (if (not (pair? mobile)) ; weight
    mobile
    (if (pair? (car mobile)) ; left side
      (+ 
        (total-product-alt (car mobile)) ; mobile
        (total-product-alt (cdr mobile)))
      (+
        0
        (* (car mobile) (total-product-alt (cdr mobile)))))))

(total-product-alt mobile-alt)
(total-product-alt branch-1-alt)
(total-product-alt branch-2-alt)
(total-product-alt sub-mobile-alt)
(define (mobile-alt? mobile)
  (and (pair? mobile) (pair? (car mobile)) (pair? (cdr mobile))))
(define (balanced-alt? mobile)
  (if (null? mobile)
    #t
    (if (mobile-alt? mobile)
      (eq? (total-product-alt (car mobile)) (total-product-alt (cdr mobile)))
      (and (balanced-alt? (cdr mobile)) (balanced-alt? (car mobile))))))
(balanced-alt? mobile-alt)
(balanced-alt? (make-mobile-alt sub-mobile-alt sub-mobile-alt))

(say "Exercise 2.30")
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else
          (cons (square-tree (car tree))
                (square-tree (cdr tree))))))
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (square-tree-map tree)
  (map (λ (sub-tree)
         (if (pair? sub-tree)
           (square-tree-map sub-tree)
           (square sub-tree)))
      tree))
(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(say "Exercise 2.31")
(define (tree-map f tree)
  (map (λ (sub-tree) 
          (if (pair? sub-tree)
            (tree-map f sub-tree)
            (f sub-tree)))
       tree))
(tree-map square (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(say "Exercise 2.32")
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map 
                     (λ (item) (cons (car s) item))
                     rest)))))

(subsets (list 1 2 3))
; the subsets is constructed as following:
; when iterating at '(): ('())
; when iterating at 3: ('() (3))
; when iterating at 2: ('() (3) (2) (2 3))
; when iterating at 1: ('() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(say "Exercise 2.33")
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(define (my-map p sequence)
  (accumulate (λ (x y) (cons (p x) y)) '() sequence))
(my-map square (list 1 2 3))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(my-append (list 1 2 3) (list 4 5 6))
(define (my-length sequence)
  (accumulate (λ (x y) (+ y 1)) 0 sequence))
(my-length (list 1 2 3))

(say "Exercise 2.34")
(define (horner-eval x coefficient-sequence)
  (accumulate (λ (this-coeff higher-terms) 
                    (+ (* higher-terms x) this-coeff))
                 0
                 coefficient-sequence))
; a0 = 1, a1 = 2, a2 = 3, x = 2
; a2x^2 + a1x + a0 = (a2x + a1)x + a0 = (3 * 2 + 2) * 2 + 1 = 17
(horner-eval 2 (list 1 2 3)) 

(say "Exercise 2.35")
(define (count-leaves t)
  (accumulate 
    +
    0 
    (map (λ (tree) 
            (cond 
              ((null? tree) 0)
              ((pair? tree) (count-leaves tree))
              (else 1))) 
         t)))
(count-leaves (list 1 (list 2 (list 3 4) 5)))

(say "Exercise 2.36")
(define (get-heads lst)
  (if (null? lst)
    '()
    (cons (caar lst) (get-heads (cdr lst)))))
(define (get-tails lst)
  (if (null? lst)
    '()
    (cons (cdar lst) (get-tails (cdr lst)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (get-heads seqs))
          (accumulate-n op init (get-tails seqs)))))
(get-heads (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(get-tails (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(say "Exercise 2.37")
(define vector-1 (list 1 2 3 4))
(define vector-2 (list 5 6 7 8))
(define matrix (list vector-1 vector-2 vector-1 vector-2))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product vector-1 vector-2)

(define (matrix-*-vector m v)
  (map (λ (mv) (dot-product mv v)) m))
(matrix-*-vector matrix vector-2)

;(accumulate-n cons '() (list (list 1 2 3) (list 1 2 3)))

(define (transpose m)
  (accumulate-n 
    cons
    '() m))
(say matrix)
(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map 
      (λ (v) (matrix-*-vector cols v)) m)))
(matrix-*-matrix matrix matrix)

(say "Exercise 2.38")
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))
; the op should have communitive property, such as *
(eq? (fold-right * 1 (list 1 2 3)) (fold-left * 1 (list 1 2 3)))


(say "Exercise 2.39")
(define (reverse-fold-left sequence)
  (fold-left (λ (x y) (cons y x)) '() sequence))
(reverse-fold-left (list 1 2 3))

(define (reverse-fold-right sequence)
  (fold-right (λ (x y) 
                 (if (null? y)
                   (cons x '())
                   (append y (cons x '())))
                 ) 
              '() sequence))
(reverse-fold-right (list 1 2 3))

(say "Exercise 2.40")
(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (prime? num)
  (define (prime-internal counter)
    (if (= counter 1)
      #t
      (and (not (= 0 (modulo num counter))) (prime-internal (- counter 1)))))
  (cond ((= num 1) #f)
        ((= num 2) #t)
        (else (prime-internal (- num 1)))))
;(prime? 2)
;(prime? 4)
;(prime? 10)
;(prime? 11)
;(prime? 17)
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (unique-pairs n)
  (flatmap
    (λ (i) 
       (map (λ (j) 
               (list i j))
            (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum 
       (filter prime-sum? (unique-pairs n))))
(prime-sum-pairs 6)
(define (remove item sequence)
  (filter (λ (x) (not (= x item)))
          sequence))
(define (permutations s)
  (if (null? s)
    (list '())
    (flatmap (λ (x) 
                (map (λ (p) (cons x p))
                     (permutations (remove x s))))
             s)))
(say "Exercise 2.41")
(define (ranged-triples n s)
  (filter 
    (λ (pair) (< (caddr pair) s))
    (map make-pair-sum (unique-pairs n))))
(ranged-triples 6 10)

(say "Exercise 2.42")
(define (safe? k positions)
  (let ((new-queen (last-pair positions)))
    (define (safe-internal n queens)
      (let ((this-queen (car queens)))
        (cond 
          ((equal? this-queen new-queen) #t)
          ((or
              (equal? (+ (car this-queen) n) (car new-queen)) ; diagonal
              (equal? (- (car this-queen) n) (car new-queen)) ; diagonal
              (equal? (car this-queen) (car new-queen))) ; same line
             #f)
          (else 
            (safe-internal (- n 1) (cdr queens))))))
    (safe-internal (- (length positions) 1) positions)))
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k)))) ; row, col

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (λ (positions) (safe? k positions)) ; (solution1, solution2, ...)
        (flatmap
          (λ (rest-of-queens) 
             (map (λ (new-row) 
                     (adjoin-position ; (append rest-of-queens (new-row k))
                       new-row k rest-of-queens))
                  (enumerate-interval 1 board-size))) ; (1 2 3 ... board-size)
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define sols (queens 8))
;sols
(length sols) ; 92 solutions in total

(say "Exercise 2.43")

(define (slow-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (λ (positions) (safe? k positions)) ; (solution1, solution2, ...)
        (flatmap
          (λ (new-row) 
             (map (λ (rest-of-queens) 
                     (adjoin-position 
                       new-row k rest-of-queens))
                  (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;(length (slow-queens 8))
; The issue is that `queen-cols` runs far slower than `enumerate-interval`, by interchange their position, the number of calls to `queen-cols` increases a lot.  

(say "Exercise 2.44")
(require sicp-pict)
(define wave einstein)
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
(paint (corner-split wave 4))

(say "Exercise 2.45")
(define (square-of-four tl tr bl br)
  (λ (painter) 
     (let ((top (beside (tl painter) (tr painter)))
           (bottom (beside (bl painter) (br painter))))
       (below bottom top))))

(define (split dir1 dir2)
  (define (split-internal painter n)
    (if (= n 0)
      painter 
      (let ((smaller (split-internal painter (- n 1))))
        (dir1 painter (dir2 smaller smaller)))))
  split-internal)
(define general-right-split (split beside below))
(define general-up-split (split below beside))


(say "Exercise 2.46")
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2)) (+ (ycor-vect vect1) (ycor-vect vect2))))
(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2)) (- (ycor-vect vect1) (ycor-vect vect2))))
(define (scale-vect factor vect)
  (make-vect (* (xcor-vect vect) factor) (* (ycor-vect vect) factor)))
(define vect1 (make-vect 1 2))
(define vect2 (make-vect 3 4))
vect1
vect2
(add-vect vect1 vect2)
(sub-vect vect1 vect2)
(scale-vect 10 vect1)

(say "Exercise 2.47")
(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))
(define (frame-origin-list frame)
  (car frame))
(define (frame-edge1-list frame)
  (cadr frame))
(define (frame-edge2-list frame)
  (caddr frame))
(define frame-list (make-frame-list 'origin 'edge1 'edge2))
(frame-origin-list frame-list)
(frame-edge1-list frame-list)
(frame-edge2-list frame-list)
(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (frame-origin-cons frame)
  (car frame))
(define (frame-edge1-cons frame)
  (cadr frame))
(define (frame-edge2-cons frame)
  (cddr frame))
(define frame-cons (make-frame-cons 'origin 'edge1 'edge2))
(frame-origin-cons frame-cons)
(frame-edge1-cons frame-cons)
(frame-edge2-cons frame-cons)

(say "Exercise 2.48") ; it's in Exercise 2.2
(define segment (make-segment vect1 vect2))
segment
(start-segment segment)
(end-segment segment)

(say "Exercise 2.49")
; https://stackoverflow.com/questions/13592352/compiling-sicp-picture-exercises-in-drracket
(require graphics/graphics)
(open-graphics)
(define frame-size 500)
(define vp (open-viewport "A Picture Language" (+ frame-size 5) (+ frame-size 5)))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))
(define (my-draw-line start end) ; convert vector to `posn`, which is how Racket receive arguments, also scale coordinates to the frame size
  (let ((scaled-start (scale-vect frame-size start))
        (scaled-end (scale-vect frame-size end)))
  (line 
    (make-posn (xcor-vect scaled-start) (ycor-vect scaled-start))
    (make-posn (xcor-vect scaled-end) (ycor-vect scaled-end)))))
(define make-frame make-frame-list)
(define origin-frame frame-origin-list)
(define edge1-frame frame-edge1-list)
(define edge2-frame frame-edge2-list)
(define (frame-coord-map frame)
  (λ (v) 
     (add-vect
       (origin-frame frame)
       (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                 (scale-vect (ycor-vect v) (edge2-frame frame))))))
(define (segments->painter segment-list)
  (λ (frame) 
     (for-each
       (λ (segment) 
          (my-draw-line
            ((frame-coord-map frame)
             (start-segment segment))
            ((frame-coord-map frame)
             (end-segment segment))))
       segment-list)))
; for outline
(define left-outline (make-segment (make-vect 0 0) (make-vect 0 1)))
(define right-outline (make-segment (make-vect 0 1) (make-vect 1 1)))
(define top-outline (make-segment (make-vect 0 0) (make-vect 1 0)))
(define bottom-outline (make-segment (make-vect 1 0) (make-vect 1 1)))
; for X
(define top-left-to-right-bottom (make-segment (make-vect 0 0) (make-vect 1 1)))
(define left-bottom-to-left-top (make-segment (make-vect 0 1) (make-vect 1 0)))
(define top-mid-to-left-mid (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))
(define top-mid-to-right-mid (make-segment (make-vect 0.5 0) (make-vect 1 0.5)))
; for diamond
(define left-mid-to-bottom-mid (make-segment (make-vect 0 0.5) (make-vect 0.5 1)))
(define right-mid-to-bottom-mid (make-segment (make-vect 1 0.5) (make-vect 0.5 1)))
; for T
(define vertical-line (make-segment (make-vect 0.5 0.4) (make-vect 0.5 0.6)))
(define horizontal-line (make-segment (make-vect 0.4 0.4) (make-vect 0.6 0.4)))

(define frame-origin-vect (make-vect 0 0))
(define frame-edge1-vect (make-vect 1 0)) ; x
(define frame-edge2-vect (make-vect 0 1)) ; y
(define my-frame (make-frame frame-origin-vect frame-edge1-vect frame-edge2-vect))

(define outline (segments->painter 
   (list 
     left-outline
     right-outline
     top-outline
     bottom-outline)))

(define X (segments->painter 
   (list 
     top-left-to-right-bottom
     left-bottom-to-left-top)))

(define diamond (segments->painter 
   (list 
     top-mid-to-left-mid
     top-mid-to-right-mid
     left-mid-to-bottom-mid
     right-mid-to-bottom-mid)))

(define T (segments->painter 
   (list 
     vertical-line
     horizontal-line)))

(define (paint painter)
  (painter my-frame))
(paint outline)
(paint X)
(paint diamond)
(paint T)

; I didn't draw wave :)


(say "Exercise 2.50")
(define (transform-painter painter origin corner1 corner2)
  (λ (frame)
     (let ((m (frame-coord-map frame)))
       (let ((new-origin (m origin)))
         (painter (make-frame
                    new-origin
                    (sub-vect (m corner1) new-origin)
                    (sub-vect (m corner2) new-origin)))))))
(define (my-flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (shrink-to-upper-right painter)
  (transform-painter
    painter 
    (make-vect 0.5 0.5)
    (make-vect 1.0 0.5)
    (make-vect 0.5 0.0)))
(define (rotate90 painter)
  (transform-painter 
    painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter
    painter
    (make-vect 0.0 0.0)
    (make-vect 0.65 0.35)
    (make-vect 0.35 0.65)))
(paint T)
(paint (shrink-to-upper-right T))
(paint (rotate90 T))
(paint (squash-inwards T))

(define (rotate180 painter)
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)))
(define flip-horiz rotate180)
;(paint (flip-horiz T))

(define (rotate270 painter)
  (transform-painter
    painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))
;(paint (rotate270 T))

(say "Exercise 2.51")
(define (my-beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              split-point
              (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.0)
              (make-vect 0.5 1.0))))
      (λ (frame)
         (paint-left frame)
         (paint-right frame)))))
;(paint (my-beside T T))
(define (my-below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-left
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              split-point))
          (paint-right
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.5)
              (make-vect 0.0 1.0))))
      (λ (frame)
         (paint-left frame)
         (paint-right frame)))))
;(paint (my-below T T))

(define (my-below-alt painter1 painter2)
  (let ((rotated1 (rotate90 painter1))
        (rotated2 (rotate90 painter2)))
    (let ((aligned (my-beside rotated1 rotated2)))
      (rotate270 aligned))))
;(paint (my-below-alt T T))


(say "Exercise 2.52")
; 2.52.a 
; Too lazy to draw

; 2.52.b
(define (right-split-alt painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split-alt painter (- n 1))))
      (beside painter smaller))))
(define (up-split-alt painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split-alt painter (- n 1))))
      (below painter smaller))))
(define (corner-split-alt painter n)
  (if (= n 0)
    painter
    (let ((up (up-split-alt painter (- n 1)))
          (right (right-split-alt painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split-alt painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))
;(paint (corner-split-alt wave 4)) ; run this in DrRacket

; 2.52.c
(define (square-limit-alt painter n)
  (let ((quarter (rotate180 (corner-split painter n))))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
;(paint (square-limit-alt wave 4)) ; run this in DrRacket

(say "Exercise 2.53")
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(x (apple sauce) y apple pear))
(list 'a 'b 'c) ; '(a b c)
(list (list 'george)) ; '((george))
(cdr '((x1 x2) (y1 y2))) ; '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; '(red shoes blue socks)

(say "Exercise 2.54")
(define (recursive-eq? x y)
  (cond ((or (pair? x) (pair? y)) ; one of them is pair
         (cond ((and (pair? x) (pair? y)) ; both of them are pair
                (if (eq? (car x) (car y))
                  (recursive-eq? (cdr x) (cdr y))
                  #f)) ; only one of them is pair
               (else #f)))
         ((or (null? x) (null? y))
          (cond ((and (null? x) (null? y) #t))
                (else #f)))
         (else (eq? x y))))
(recursive-eq? '(this is a list) '(this is a list))
(recursive-eq? '(this is a list) '(this (is a) list))

(say "Exercise 2.55")
(car ''abracadabra) 
; | 
; | is equivalent to
; V
(car (quote (quote abracadabra)))

(say "Exercise 2.56")
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define base cadr)
(define exponent caddr)
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp) (- (exponent exp) 1)))
           (deriv (base exp) var)))
        (else 
          (error "unknown expression type: DERIV" exp))))
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(** x 2) 'x)


