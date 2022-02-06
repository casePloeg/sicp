#lang sicp
;; Section 3.2 -- The Environment Model of Evaluation

;; substitution model: to apply a compound procedure to arguments, evaluate the body
;; of the procedure with each formal parameter replaced by the corresponding argument

;; once assignment is in the language, substitution is no longer enough

;; introducing Environments
;; environment: a sequence of frames
;; frame : table of bindings
;; binding : associate variable names with corresponding values
;; A frame can have at most one binding for any variable
;; A table can be empty

;; Each frame points to its enclosing environment, unless the frame is global (top of stack)
;; values of variables are determined by the *lowest* level frame with a binding to that
;; variable
;; if no frame specifies a binding, the variable is unbound in the environment

;; A -> {z: 6, x: 7} -C-> {x: 3, y: 5}
;; B -> {m: 1, y: 2} -D-> {x: 3, y: 5}
;; With respect to environment A, x = 7, the binding x: 7 *shadows* the x: 3 binding
;; With respect to environment C, x = 3
;; With respect ot environment B, x = 3 (search for x binding in next frame)

;; in this model, expressions are meaningless and cannot be evaluated outside an
;; environment

;; the interpreter creates a global environment that includes values for the symbols of
;; primitive procedures (+, -, *, ...)

;; 3.2.1 The Rules for Evaluation
;; evaluate the subexpressions of the combination
;; apply the value of the operator subexpression to the values of the operand subexpressions

;; procedure object: code + pointer to environment
(define square (lambda (x) (* x x)))
;; code = lambda expression
;; env = whatever env the code was in when evaluated, binding square to that expression

;; define : add a binding // change a binding

;; to apply a procedure to arguments: create a new env containing a frame that binds
;; the params to the arguments. The enclosing env is the env specified by the procedure.
;; Within the new env evaluate the procedure


;; 3.2.2
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))



;; ex 3.9
;; refer to section 1.2.1 , linear recursion
(define (factorial-1 n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; (f 6)
;; global: {f: lambda-exp}
;; E1 : {n: 6} -> global : {f: l-exp} -> E2 : {n: 5} ... -> global : {f: l-exp} -> E6

(define (factorial n)
  (fact-iter 1 1 n))

;; fact-iter uses tail-recursion, since we *only* have to return the result of
;; fact-iter() each time, we don't have to keep track of previous frames before returning
;; tail-recursion happens on the interpreter level
(define (fact-iter product
                   counter
                   max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;; (f 6)
;; E1 : {n: 6} -> global -> E2 : {p: 1, c: 1, m: 6} -> global -> E3: {p: 1, c: 2, m:6}
;; ... -> E8 : {p: 720, c: 7, m: 6}
;; ret: 720

;; Section 3.2.3 Frames as the Repository of Local State

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds")))

;; key insight: the *let* expressions we used earlier to help define local state
;; were actually just lambda expressions
(define W1 (make-withdraw 100))
;; bind W1 to the procedure object returned by the make-withdraw() procedure
;; now W1 points to the code + persistent environment (which holds local state)


;; ex 3.10
;; consider make-withdraw() using a let expression instead of arguments to create
;; local state. how does the environment structure change?
;; using the let expression introduces an intermediate procedure object
;; so we have make-withdraw(env) : {initial-amount} & balance(env) : {balance}
;; and the procedure object returned by make-withdraw() points to balance(env),
;; which is enclosed by make-withdraw(env)
