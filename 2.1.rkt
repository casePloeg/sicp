#lang sicp
;; 2.1.1 arithmetic operations for rational numbers
;; assume we can already construct rationals
;;        we have a way of extracting (selecting) its numerator & denominator

;; pairs in Lisp
;; (define x (cons 1 2))
;; (car x) --> 1
;; (cdr x) --> 2
;; pairs as a primitive can be used to build lots of complex data structures: list-structured data

;; powerful strategy of synthesis: wishful thinking
;; what could I do if I had already had X?

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (add-rat x y)
  (make-rat (+
             (* (numer x) (denom y))
             (* (denom x) (numer y)))
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

;; (define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))


;; ex 2.1
;; define better make-rat that handles pos and neg args

(define (make-rat n d)
  (cond ((and (>= n 0) (>= d 0)) (cons n d))
        ((and (< n  0) (< d  0)) (cons (abs n) (abs d)))
        (else (cons (* -1 (abs n)) (abs d)))))

;; abstraction barriers, define interfaces for higher-level programs
;; -- programs that use rational numbers --
;; (rational numbers in problem domain)
;; -- add-rat  sub-rat ... --
;; (rational numbers as numerators and denominators)
;; -- make-rat numer denom --
;; (rational numbers as pairs)
;; -- cons car cdr --
;; (however pairs are implemented)


;; constrain dependence on representation to an interface layer
;; means not having to update higher level interfaces

;; ex 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (let ((x1 (x-point start))
          (y1 (y-point start))
          (x2 (x-point end))
          (y2 (y-point end)))
      (cons (average x1 x2) (average y1 y2)))))
 
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; (define p1 (make-point 0 0))
;; (define p2 (make-point 5 10))
;; (define s1 (make-segment p1 p2))
;; (define mid (midpoint-segment s1))
;; (print-point mid)
;; (5/2, 5)

;; ex 2.3 implement representation for rectangles , compute perimeter and area
;; implement two different representations, perimeter and area procedures should work either way!
;; what is a "representation"?
(define (square x) (* x x))
;; ex 1.46
;; create a procedure that abstracts the concept of iterative improvement
;; use this abstract to redefine the sqrt function
(define (iterative-improve good-enough? improve)
  ;; returns a function that takes a guess and iteratively improves that guess
  (lambda (x)
    (define (iter-guess x)
      (if (good-enough? x)
          x
          (iter-guess (improve x))))
    (iter-guess x)))

;; what is the sqrt of x ?
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ;; iteratively improve upon an initial guess of 1.0
  ((iterative-improve good-enough? improve) 1.0))

(define (length s)
   (let ((start (start-segment s))
        (end (end-segment s)))
    (let ((x1 (x-point start))
          (y1 (y-point start))
          (x2 (x-point end))
          (y2 (y-point end)))
      (sqrt (+ (square (abs (- x1 x2)))
               (square (abs (- y1 y2))))))))

;; rectangle representation layer
(define (make-rect p1 p2 p3 p4)
  (cons (make-segment p1 p2) (make-segment p1 p3)))

(define (get-s1 r)
  (car r))

(define (get-s2 r)
  (cdr r))

;; width and height interface layer
(define (rect-w r)
  (length (get-s1 r)))

(define (rect-h r)
  (length (get-s2 r)))

;; area and perimeter layer
(define (area r)
  (* (rect-w r) (rect-h r)))

(define (perimeter r)
  (+ (* 2 (rect-w r)) (* 2 (rect-h r))))



;; 2.1.3 what is data?

;; defined by some collection of selectors and constructors,
;; with specified conditions that must be fulfilled to be a valid representation
;; rational numbers: if x = (make-rat n d) , then (numer x) / (denom x)  = n / d
;;; pairs: if z = (cons x y) then (car z) = x && (cdr z) = y

;; implement pairs using only procedures
;; using *message passing* technique
"
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           (error Argument not 0 or 1: CONS m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))
"
;; not an actual implementation, for efficiency reasons, but it's *possible*

;; ex 2.4
;; alternative pair represenation
"
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
"
;; ex 2.5 let a and b be nonnegative integers
;; 2^a * 3^b
"
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (if (= (modulo z 2) 0) (+ 1 (car (/ z 2))) 0))

(define (cdr z)
  (if (= (modulo z 3) 0) (+ 1 (cdr (/ z 3))) 0))
"
;; ex 2.6
;; church numerals

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define (mult a b)
  (lambda (f)
    (lambda (x)
      ((a (b f)) x))))

(define three (add one two))
(define four (add two two))

;; two + four = 6 !! this is epic
(((add two four) (lambda (x) (+ x 1))) 0)

;; Interval Arithmetic

;; rp = 1 / (1/r1) + (1/r2) , upto a tolerance (upper and lower bounds)

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))




(define (div-interval x y)
  (cond ((and (>= (upper-bound y) 0) (<= (lower-bound y) 0)) (error "possible div by zero"))
        (else (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y)))))))
  
;; ex 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

;; ex 2.8
(define (sub-interval x y)
  (make-interval
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))))


"
(width (add-interval a b))
(average (+ (upper-bound x) (upper-bound y)) (+ (lower-bound x) (lower-bound y)))
(+ (average (upper-bound x) (lower-bound x)) (average (upper-bound y) (lower-bound y)))
(+ (width a) (width b))
"

" x = [1, 3] , y = [2, 8] , x*y = [2, 24] , width x = 1 , width y = 3, width x*y = 11 != 1 * 3 "

;; ex 2.10
;; error division when interval goes through zero

;; ex 2.11
;; annoying
;; 9 cases by signs of endpoints:
;; where + -> interval is overall positive
;; where - -> interval is overall negative
;; where 0 -> interval includes 0 (has positives and negatives)
;; +, + (x0 * y0) (x1 * y1)
;; +, - (x1 * y0) (x0 * y1)
;; +, 0 (x1 * y0) (x1 * y1)
;; -, +
;; -, -
;; -, 0
;; 0, +
;; 0, -
;; 0, 0

(define (make-cetner-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (make-center-percent c t)
  (make-interval (- c (* c t)) (+ c (* c t))))

(define (percent i)
  (/ (width i) (center i)))

;; ex 2.13
;; small tolerance, simple formula for approx percentage tolerance of product of intervals
;; (a + w) * (c + w)
;; (a + at) * c + ct)
;; (ac + act + cat + atct)
;; ~ (ac + ac (t + t))
;; approx just take the sum of tolerances

(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))
    
;; ex 2.15
;; is par1 better than par2 when considering error bounds?
;; at small percentages we are likely seeing round off errors
;; propogated by division, less division is probably better so I'd go with par1

;; ex 2.16
;; our system has no concept of inverses or identity functions
;; (div i i) != one
;; or for example, the distrubitive law of multiplication does not hold in
;; our interval arithmetic system

;; note: this *does not* likely have to do with numerical analysis, as the percentages are
;; probably not that small to cause floating point truncation


