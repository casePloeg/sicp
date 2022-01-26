;; https://mitpress.mit.edu/sites/default/files/sicp/psets/index.html
#lang sicp
;; evaluation rules make up the syntax of a language
;; the more abstracted your evaluation is, the less syntax you will have

;; expressions, when evaluated return a value -- everything is an expression in Lisp
;; this is *not* true for every langauge, in Python not everything is an expression

;; there is a generic evaluation rule for expressions
;; there is a special rule for define , define is an expression

;; special syntax that provide convience but can be expressed more genericly are called syntactic sugar
;; can cause more trouble than it's worth in large systems - uniformity makes things easy to reason about

;; abstraction of values --> variable definition
;; abstraction of combination of values --> procedure definition
;; possible to define procedures without naming them and to name prodecures that have already been defined

;; the last expression of a procedure is responsible for the value returned --> no early returns

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.5

(define (square x) (* x x))
;; evaluate a procedure:
;; (f 5)
;; (sum (+ a 1) (* a 2))
;; (sum (+ 5 1) (* 5 2))
;; (sum (6) (10))
;; (+ (square 6) (square 10))
;; (+ (* 6 6) (* 10 10))
;; (+ 36 100)
;; 136

;; normal order -- substitute expressions until only primitive operators are left, then evaluate
;; applicative order -- evaluate operands then apply them to operators
;; both orders will give same results for legitimate values ** possible for one to fail and the other to succeed

;; Lisp is applicative, normal gets confusing once substitution breaks down but can be useful
;; when dealing with streams

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.6
;; (cond (<p1> <e1>)) <e1> can be a sequence of expressions
;; if no predicate is true, then the value of the cond expression is undefined
;; (if <predicate> <consequent> <alternative>) , special form
;; logical operators, and or not exist in Lisp -> and / or are special forms because they have lazy evaluation

;; ex 1.1
10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16

;; ex 1.2
(/
 (+ 5 2 -3 6 (/ 4 5))
 (* 3
    (- 6 2)
    (- 2 7)))

;; ex 1.3
;; check if first arg is the smallest one, if it is return value
;; otherwise rotate arguments until smallest is found
(define (sum-of-largest-sqs a b c)
  (cond ((and (< a b) (< a c)) (+ (* b b) (* c c)))
        (else (sum-of-largest-sqs c a b))))
(sum-of-largest-sqs 1 2 3)
(sum-of-largest-sqs 2 1 3)
(sum-of-largest-sqs 2 3 1)

;; ex 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; if b > 0 , then the expression evaluates to the + operator , - otherwise

;; ex 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))
;; applicative order -- evaluate all operands -> expand p -> infinite loop
;; normal order -- evaluate operands only when needed -> x == 0 -> return 0

;; mathematics is declarative knowledge (what is)
;; computing is imperative knowledge (how to)
;; to know a program is *correct* is declaritive knowledge
;; very high-level --> program in declaritive statements (as much as possible, not generally)

;; you have imperative knowledge if you can produce a result when asked a question
;; you have declaritive knowledge if you can fact-check someone else's answer to a question

;; newtons method
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x) ;; this predicate show-cases our declaritive knowledge of square roots
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; any purely numerical program could be implemented with the above techniques ^^
;; iteration is possible using procedural calls (recursion), just keep track of an increasing index parameter

;; ex 1.6
;; "Why can't I just define it as an ordinary procedure in terms of cond?"
;; -- you *can* use cond like an if, but defining that as a procedure will have problems, the special form of only
;; evaluating one of the clauses based on the predicate breaks applicative order
;; "is 'if' an ordinary procedure?" NO
;; because new-if is a function, all the parameters will be evaluated in applicative order,
;; the predicate will never cause an early exit and the program infinitely loops

;; ex 1.7
;; very small and large numbers will not work with current implementation.
;; we may want small numbers to be accurate past 0.001
;; large numbers will suffer from lack of precision and the process can infinitely loop
(define (good-enough2? guess x)
  (= guess (improve guess x)))
;; if the guess can no longer be improved then we are finished (go to max precision)
;; this works better because it takes into account the precision of the calculation instead of the degree of
;; correctness. We *know* that the answer will improve over time so we don't check that it is correct, we check
;; whether this is still improving to be done

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)
                      x)))
;; the meaning of a procedure must not be affected by the names of the parameters
  ;; names are localized

;; prodecure "formal" params are bounded variables to the scope of the procedure, their names are independent
  ;; from the functionality of the prodecure
  ;; variables that are out of scope are called "free" and the function does depend on their definitions

;; nested definitions are called a "block structure" and allows for private procedures
  ;; using this we can pass around less variables --> lexical scoping "looking up in the environment chain"
  ;; embedded defintions must come first! otherwise you're asking for trouble
  
;; does reusable code actually save time?
;; every time I *change* reusable code I have to make sure I'm not breaking everyone else's use cases
;; backwards compatibility is probably important
;; deprecation seems like torture

  