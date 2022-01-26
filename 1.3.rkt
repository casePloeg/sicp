#lang sicp
;; Formulating Abstractions with Higher-Order Procedures
;; -----------------------------------------------------
;; procedures whose parameter types can only be primitives are restrictive
;; we want to be able to define procedures that accept procedures as *arguments*
;; we want to be able to return procedures as values
;; we want procedures that can *manipulate* other procedures
;; being able to do so will give us much more expressive power in the language
(define (cube x) (* x x x))

"
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
"
"
(define (sum-cubes a b)  
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))
"
;; Leibniz series to compute pi
"
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))
"
;; the previous three procedures are all similar to each other
;; they differ by name, a defintion of each term in the series, and a transition to the next "index"
;; we want be able to define "sigma" notation , so we can think about the concept of summation itself
;; and not the underlying f(n)

;; higher order function, defines summation:
;; recursive process
"
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
"
(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define id identity)

(define (sum-integers a b)
  (sum identity a inc b))

;; I think this can be made more concise with lambda functions
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; summation itself if an example of a further abstraction: accumulation
;; how to abstract: replace every value used in a procedure with a parameter
;; some abstractions are useful, some are frivolous
;; -- perhaps the middle ground of abstraction is where things are most murky
;; ---- or maybe each layer of abstraction should feel like an improvement on the last

;; ex 1.29
;; implementing Simpson's Rule to estimate integration
(define (sum-simpson n f a b)
  (define (h) (/ (- b a) n))
  (define (y k) (f (+ a (* k (h)))))
  (define (simp-term k)
    (* (y k)
       (cond ((or (= k 0) (= k n)) 1)
             ((even? k) 2)
             (else 4))))
    
  (* (/ (h) 3) (sum simp-term 0 inc n)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

;; ex 1.30 write the sum procedure using an iterative process
"
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)(+ result (term a)))))
  (iter a 0))
"
;; ex 1.31 write a product procedure analogous to sum, implement both recursive and iterative versions
"
(define (product term next a b)
  (if (> a b)
      1
      (*
       (term a)
       (product term next (next a) b))))
"
(define (product term next a b result)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial x) (product identity inc 1 x 1))

;; ex 1.32
;; (accumulate combiner null-value term a next b)
;; product --> combiner: * , null-value: 1
;; sum     --> combiner: + , null-value: 0
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
        
(define (sum term a next b)
  (accumulate + 0 term a next b))

;; ex 1.33
(define (filtered-accumulate combiner null-value term a next b filter?)
  (cond ((> a b) null-value)
        ((filter? (term a)) (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter?)))
        (else (filtered-accumulate combiner null-value term (next a) next b filter?))))
;; method 1: find the number's divisors
(define (smallest-divisor n)
  (find-divisor n 2))

;; check each possible divisor by checking successive integers
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (inc test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-prime a b)
  (filtered-accumulate + 0 id a inc b prime?))

;; https://sarabander.github.io/sicp/html/1_002e3.xhtml#g_t1_002e3_002e1
;; 1.3.2 Constructing Procedures Using Lambda
(lambda (x) (+ x 4))
;; think of "lambda" as "make-procedure"
;; this name is conventional based on lambda calculus developed by Alonzo Church as a way of formalizing function behaviour
(define p (lambda (x) (+ x 4)))
;; auxilary lambda functions can be used to compute intermediate values that will be reused (local vars)
;; make the lambda procedure, give it the arguments, execute it
"
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
"
;; special form "let" does this slightly more elegantly by using name expression pairs
;; no new functionality, just syntactic sugar for lambda
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; it's possible to use internal definitions to have the same effect as let
;; but we prefer let because it's harder to mess up. (The scoping is more well defined?)

;; ex 1.34
;; (define (f g) (g 2))
;; (f f) --> (f 2) --> (2 2) ? 2 is not a procedure

;; https://sarabander.github.io/sicp/html/1_002e3.xhtml#g_t1_002e3_002e1
;; 1.3.3 Procedures as General Methods
;; 1.35 calculate golden ration x -> 1 + 1/x
(define tolerance 0.001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))

;; 1.36
;; modify fixed-point to display approximations
(define (average x y) (/ (+ x y) 2))

(define (x-x)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2))

;; 1.37
(define (cont-frac n d k)
  (let ((dk (d k)) (nk (n k)))
    (display k)
    (display " ")
    (display dk)

    (newline)
    (define (helper max k)
      (cond ((= k max) 0)
            (else (/ (n k) (+ (d k) (helper max (+ k 1)))))))
    (helper (+ k 1) 1)))

(cont-frac (lambda (i) 1.0)
           (lambda(i)
             (cond ((= i 2) 2)
                    ((and (> i 4) (= (modulo (- i 2) 3) 0)) (* (+ (/ (- i 2) 3) 1) 2))
                    (else 1)))
           10)

;; 1.3.4 Procedures as Returned Values
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

((average-damp square) 10)

"
(define (sqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))
"
(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)
      (/ x (square y))))
   1.0))

;; implementing Newton's Method
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
;; be alert to opportunities for building more powerful abstractions
;; first-class progamming elements:
;; may be named by variables
;; may be passed as args to procedures
;; may be returned as results of procedures
;; may be included in data structures
;; Lisp, unlike other languages, makes procedures a first-class element
;; ex 1.41
(define (double f)
  (lambda (x) (f (f x))))
;; (((double (double double)) inc) 5)
;; (( ((double double) (double double)) inc) 5)
;; 5 + 16 -> 21


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

;; It would be clearer and less intimidating to people learning Lisp if a name more obvious than lambda,
;; such as make-procedure, were used. But the convention is firmly entrenched.
;; -- this is a funny quote imo. there seems to be a lot more people today who would be willing to rename
;; -- stuff like this. I wonder if it would have been easier or harder to make such a change in the 80s or now
