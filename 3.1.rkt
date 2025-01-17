#lang sicp

;; Assignment and Local State

;; an object "has state" if its behaviour is influenced by its history
;; characterized by state-variables
;; - required to contain enough information about the object's history
;; -- so that current behaviour can be calculated correctly
;; recording the entire history is not always necessary, just need to accurately
;; reflect the effects of history

;; in a system, objects with influence each other through interaction

;; view: system can be grouped into closely coupled subsystems that
;; are only loosely coupled with other subsystems
;; (compress individual objects into larger groups of influence)

;; state variables update over time
;; modeling state variables by  ordinary symbolic names
;; requires the introduction of the *assignment operator*
;; allowing change of value associated with a name (var)

;; state variables imply: the same procedure can produce
;; multiple outputs given identical inputs

;; without assignment, procedures are purely specification
;; for computing mathematical functions

;; balance defined as a global var,
;; can be changed by procedures outside of withdraw
;; is not 'local' in the way we desire,,,
;; the bank balance is easily hacked
(define balance 100)

;; set! , changes the value of the variable
;; begin, causes all expressions within to evaluate,
;;        and returns the last one
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

;; this time balance is encapsulated
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

;; local state makes it so the substitution model of evaluation
;; is not enough to interpret a procedure.
;; substitution: evaluate the body of the procedure with the params
;; replaced by their values

;; variation:
;; formal parameters are already local variables
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
;; both W1 and W2 are independent objects, intialized with $100

;; now we have a bank account object
;; can withdraw or deposit
;; uses message-passing style to generalize


;; filter() + make-joint() are additions for ex 3.7
(define (filter pred sequence)
  (define (iter res sequence)
    (cond ((null? sequence) res)
          (else (if (pred (car sequence))
                    (iter (cons (car sequence) res) (cdr sequence))
                    (iter res (cdr sequence))))))
  (iter '() sequence))

(define (make-joint acc old-pass new-pass)
  (begin ((acc old-pass 'add-pass) new-pass)
         acc))
         
(define (make-account balance secret)
  (let ((incorrect 0)
        (passwords (list secret)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops amount)
      "ring ring mfer")
    (define (add-pass new-pass)
      (set! passwords (cons new-pass passwords)))
    (define (check-pass pass)
      (< 0 (length (filter (lambda (x) (eq? x pass)) passwords))))
    (define (dispatch password m)
      (cond ((check-pass password)
             (set! incorrect 0)
             (cond
               ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               ((eq? m 'add-pass) add-pass)
               (else (error "Unknown request: MAKE-ACCOUNT" m))))
            ((>= incorrect 7) call-the-cops)
            (else
             (set! incorrect (+ incorrect 1))
             (error "Incorrect password"))))
    dispatch))
  

;; ex 3.1
(define (make-accumulator total)
  (lambda (inc)
    (begin
      (set! total (+ total inc))
      total)))

;; ex 3.2
(define (make-monitored f)
  (let ((call-count 0))
    (define (default x)
      (begin
        (set! call-count (+ call-count 1))
        (f x)))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) call-count)
            (else (default m))))
    dispatch))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)

;; ex 3.3
(define acc2
  (make-account 100 'secret-password))

((acc2 'secret-password 'withdraw) 40)

;; ((acc2 'some-other-password 'deposit) 50)

;; ex 3.4

;; implement call-the-cops procedure if incorrect password is given
;; 7 times already

;; 3.1.2 The benefits of Introducing Assignment
;; benefit: programmers can delegate "memory" tasks to the computer, instead of
;; keeping track of things by hand. This is a great productivity boost, as it
;; decouples stateful prodecures from functional producures within a system

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;; ex 3.5
;; use Monte Carlo integration to estimate pi by finding the area
;; of a circle

;; area of circle = pi * radius * radius
;; --> pi = area if radius = 1
;; area of rectangle to surrond the unit circle = 2w * 2h = 4
;; pi / 4 chance that a random point with the rectangle is in the unit circle
"
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials 
                          cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))
"
(define (estimate-integral trials)
  (* 4 (monte-carlo trials rect-test)))

(define (rect-test)
  (let ((x (random-in-range -1.0 1.0))
        (y (random-in-range -1.0 1.0)))
    (<= (+ (* x x) (* y y)) 1)))

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

;; ex 3.6
(define (rand-update x) (+ x 1))
(define (make-rand)
  (let ((x 0))
    (define (dispatch msg)
      (cond ((eq? msg 'generate) (set! x (rand-update x)) x)
            ((eq? msg 'reset) (lambda (val) (set! x val)))))
    dispatch))
;; (define rand (make-rand))
;; (rand 'generate) --> 1
;; (rand 'generate) --> 2
;; ((rand 'reset) 27) -> set local var x to 27
;; (rand 'generate) --> 28

;; programming without assignments == functional programming
;; the modeling of mathematical functions
;; assignment changes the idea of a variable from just being a name
;; to representing a place where value is stored & that the value
;; at that place can change ~pointers~

;; because underlying values can change, substitution and therefore
;; simplification become hard to reason about
;; "equals can be substituted for equals" -- referential transparency

;; we must now worry about side-effects / aliasing bugs


;; programming with assignments is called *imperative programming*

;; compare the following:
(define (factorial-1 n)
  (define (iter product counter)
    (if (> counter n)
         product
         (iter (* counter product)
               (+ counter 1))))
  (iter 1 1))

(define (factorial-2 n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

;; version 2 is written correctly, but if the two set! statements were swapped
;; we would get a much different answer. Because the values of the variables can
;; change, ordering of expressions is much more important in imperative programs.
;; ordering of assignments is a non-issue in functional programs

;; ex 3.7 - added additions to original make-account procedure

;; ex 3.8
;; with assignments, ordering of argument expansion is important
;; consider the following:
;; (+ (f 0) (f 1))


;; procedure that returns a lambda, re-initializes state on each call -- does not work
(define (bad-f)
  (let ((called 0))
    (lambda (x)
      (if (= 0 called)
          (begin
            (set! called 1)
            x)
          0))))


;; define a procedure *using* a lambda to get access to state, initializes state once -- works as intended
(define f
  (let ((called 0))
    (lambda (x)
      (if (= 0 called)
          (begin
            (set! called 1)
            x)
          0))))
