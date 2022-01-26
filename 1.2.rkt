#lang sicp
;; elements of programming: primitive operations, combinations or operations,
;; abstraction of combinations into abstract procedures

;; we now know the "rules" of the game, but not the necessary tactics, opening theory, and strategy
;; parallel chess to programming -- learning games <-> programming ... etc

;; required ability to predict outcomes before committing to an action
;; need to be able to visualize the processes of different procedures, what does the call stack look like?

;; factorials: n! = n * (n - 1)!
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
;; "shape of expansion" --> deferred operations --> recursive process
;; number of deferred operations grow proportionally to n, so linear

;; factorials: n! = (((1) * 2) * 3) ...
;; product = counter * product
;; counter = counter + 1
(define (factorial2 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter n)
  (if (> counter n)
      product
      (fact-iter (* product counter)
                 (+ counter 1)
                 n)))
;; iterative process, does not "grow or shrink"
;; all we need is the state variables produce, counter, n
;; and state transition rules
;; and optionally a "terminal state"
;; we will need n steps to terminate, so linear

;; iterative processes can be stopped and then restarted from a checkpoint, you can do everything in memory
;; recursive processes require a "one-shot" execution because the interpreter is storing info during execution
  ;; requires a stack

;; recursive process != recursive procedure (process refers to the execution shape,
  ;; procedure refers to the syntax

;; an iterative process is *actually* iterative in the fact that its memory usage is constant
  ;; this is possible in Scheme using tail-end recursion, but is not possible in a language like C
;; tail end recursion has long been known as a compiler optimization trick
;; https://stackoverflow.com/questions/33923/what-is-tail-recursion

;; ex 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
;; process: linear recursive
;; (+ 3 5)
;; (inc (+ 2 5))
;; (inc (inc (+ 1 5)))
;; (inc (inc (inc (+ 0 5))))
;; (inc (inc (inc 5)))
;; (inc (inc 6))
;; (inc 7)
;; 8

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
;; process: linear iterative (by tail recursion)
;; (+ 3 5)
;; (+ 2 6)
;; (+ 1 7)
;; (+ 0 8)
;; 8

;; ex 1.10 Ackermann Function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 1 10)
;; (A (- x 1) (A x (- y 1))
;; (A 0 (A 1 9))
;; (* 2 (A 1 9))
;; (* 2 (* 2 (A 1 8)))
;; ...
;; (* 2 (* 2 ... (2) ... )) == 2 ^ 10 = 1024

;; (A 2 0)
;; 0

;; (A 2 1)
;; 2

;; (A 2 2)
;; (A 1 (A 2 1))
;; (A 1 2)
;; (A 0 (A 1 1))
;; (A 0 2)
;; (* 2 2)
;; 4

;; (A 2 3)
;; (A 1 (A 2 2))
;; 2 ^ (A 2 2))
;; 2 ^ 2 ^ (A 2 1)
;; 2 ^ 2 ^ 2
;; (A 1 4)
;; 2 ^ 4 = 16

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 16)
;; (2 ^ (2 ^ (2 ^ 2)))

;; 2 ^ 16 = 65536

(define (f n) (A 0 n)) ;; (f n) = 2n
(define (g n) (A 1 n)) ;; (g n) = 2^n
(define (h n) (A 2 n)) ;; (h n) = 2^(2 ^ (n times))

;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (2 ^ 2))
;; (A 2 4)
;; --> 2 ^ 2 ^ 2 ^ 2
;; 2 ^ 16 = 65536

;; Common pattern of computation: tree recursion
;; "patterns of computation" -- model your data to fit the preferred computation model
;; recursive defintion of Fibonacci:
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
;; fib 2 --- fib 1 -- 1
;;       |-- fib 0 -- 0

;; branches split into two at each level --> fib calls itself twice each time invoked
;; time to compute tree process will grow with the number of nodes
;; space to store tree process will grow with depth of tree

;; linear iteration process for calculating fibonacci
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


;; trees are incredibly useful on hierachrical data rather than numbers
;; -- example: the evaluting expressions is tree-like
;; can also be useful in helping to understand and design, brute-force before optimization

;; recursive can be more declaritive in nature
;; iteration requires imperative knowledge of state variables and transitions


;; Counting Change
;; how many differnt ways can we make change of $1.00 given .5, .25. .1, .05. and 0.01 coins?
;; can we write a *iterative* procedure to compute this?

;; number of ways to change amount a using n kinds of coins
;; = (dont use first coin) number of ways to change amount a using all but first kind of coin +
;;  (uses first coin) number of ways to change a - d using all n kinds of coins, d = denomination of first coin

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))



(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
(count-change 100) ;; 292

;; one way to optimize is tabulation / memoization of values
;; dynamic programming
;; possible to convert this exponential time algorithm into polynomial time


;; state: make change for amount a
;; (incorrect) transition: dp[a] = sum( dp[a - c] for c in coins) <<<<---- THIS COUNTS SEQUENCES AND WILL DOUBLE COUNT

;; (correct) transition: for c in coins: for a in amount: dp[a] += dp[a-c] <--- unique ordering (ADD A NEW COIN EACH ITERATION)
;; could implement this but I don't know a great way to keep track of the table efficiently yet
;; see ex 3.27

;; ex 1.11
(define (recursive-f n)
  (if (< n 3) n (+ (recursive-f (- n 1))
                   (* 2 (recursive-f (- n 2)))
                   (* 3 (recursive-f (- n 3))))))

(define (f n)
  (if (< n 3) n (recursive-f n)))

;; keep last 3 calculations in state
(define (iter-f n a b c)
  (if (= n 0) (+ a (* 2 b) (* 3 c)) (iter-f (- n 1) (+ a (* 2 b) (* 3 c)) a b)))

(define (f n)
  (if (< n 3) n (iter-f (- n 3) 2 1 0)))

;; ex 1.12

(define (pascal row col)
  (cond ((or (= col row) (= col 1)) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

;; ex 1.13
;; just do the math nerd

;; ORDERS OF GROWTH -- ASYMPTOTIC COMPLEXITY

;; R(n) is O(f(n)) if k1*f(n) <= R(n) <= k2*f(n) for large n

;; ex 1.14
;; draw tree for count-change 11 cents
;; a = 11 , n = 2
;; a = 11, n = 1 -- a = 10, n = 2
;; a = 11, n = 0 -- a == 6, n = 1 -- a = 10, n = 1 -- a = 9, n = 2
;; ret 0 -- a = 6, n = 0 -- a = 1, n = 1-- a = 5, n =1 -- a = 10, n = 0

;; for each new coin, we need to do n * all of the previous work
;; n = 1 -> R(n) ---> time : R(n^k) = k ^ log_k_(n)
;; time: R(n^k) space: R(n) (max depth of tree)
 
; ex 1.15
;; number of calls for (sine 12.15) = O(log_3_(12.15))
; > (/ 12.15 3)
; 4.05
; > (/ 4.05 3)
; 1.3499999999999999
; > (/ 1.35 3)
; 0.45
; > (/ 0.45 3)
; 0.15
; > (/ 0.15 3)
; 0.049999999999999996
;; 5 calls total

;; space and time should be O(log n) , one recursive call and the final result will come when the first call finishes
;; size of n decreases by a constant factor each call

;; EXPONENTIATION
;; ex 1.16 design a procedure to iteratively calculate the b^n

;; invariant: a * b^n = b^n for every iteration
(define (fast-expt b n)
  (expt-iter b n a))

(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (* b b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* a b)))))

;; ex 1.17
;; multiplication is repeadeted addition
;; * a b = (double (* a (halve b 2))) if b is even
;; * a b = (+ (* a (- b 1)) b) if b is odd

(define (double a)
  (+ a a))
(define (halve a)
  (/ a 2))
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ (fast-mult a (- b 1)) a))))

;; ex 1.18 ITERATIVE FAST MULTIPLICATION
;; total + ab = total' + ab for all iterations
;; when b = 0, total = ab
;; progressively add more and more value into a
;; then off load it into total when necessary (b = 1 will eventually happen)
(define (iter-mult a b total)
  (cond ((= b 0) total)
        ((even? b) (iter-mult (double a) (halve b) total))
        (else (iter-mult a (- b 1) (+ total a)))))


;; ex 1.19 log(n) fibonacci
;; this a system of linear equations -> can solve using 2x2 matrix multiplication
;; T(p q)(a b) = a' = bq + aq + ap, b' = bp + aq
;; T^2(p q)(a b) = a'' = b'q +a'q + a'p, b'' = b'p + a'q
;;                     = (bp + aq)q + (bq + aq + ap)(q + p) , (bp + aq)p + (bq + aq + ap)q
;;                     = b(qq + 2qp) + a(qq + 2qp) + a(pp + qq), b(pp + qq) + a(qq + 2qp)
;; p' = (pp + qq) , q' = (qq + 2qp)
;; if p = 0, q = 1 --> p' = 1 q' = 1  so Fib(n + 3) , Fib(n + 2) = Fib(n) + 2Fib(n + 1), Fib(n) + Fib(n + 1)


;; GREATEST COMMON DIVISOR (GCD) -- euclid's algorithm
;; oldest know "generalized" algorithm
;; logarithmic growth , related to fibonacci numbers
;; lame thm: if euclid algo requires k steps to compute gcd of some pair,
;; then the smaller number in the pair must be greater than or equal to the kth fibonacci number
;; this thm is enough to claim gcd is O(log(n))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; ex 1.20
;; it takes 18 remainder operations to calculate (gcd 206 40) if using normal order evaluation
;; this function is probably is some function of the fibonacci numbers as well
;; it only takes 4 if using applicative order


;; TESTING FOR PRIMALITY
;; O(sqrt(n)) vs. O(log(n))
;; <-> O(1/sqrt(n)) vs. O(1/n) (by L'Hospital)
;; --> sqrt(n) grows faster than log(n)

;; method 1: find the number's divisors
(define (smallest-divisor n)
  (find-divisor n 2))

;; check each possible divisor by checking successive integers
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;; if n is prime it must have a divisor <= sqrt(n)
;; prime has an order of growth: O(sqrt(n))

;; method 2: Fermat Test
;; Fermat's Little Thm: if n is prime and a is positive integer < n, then a raised to the nth power is
;; congruent to a % n

;; congruent modulo n --> both numbers have the same remainder when divided by n

;; if n is not prime, then *most* numbers a < n will not satisfy the relation (probably)

;; implement the Fermat test:
;; expmod computes (base ^ exp) mod m
;; uses the fact that (A * B) mod C = (A mod C * B mod C) mod C
;; you can prove this using the quotient remainder thm
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

;; if n fails the Fermat Test, then n is not prime
;; if n passes, we know n is likely to be prime
;; probabilistic algorthim: we can decrease the chance of error each iteration
;; --- cryptography (RSA) uses probability and primes a lot

;; ex 1.21
;; (smallest-divisor 199)
;; 199
;; (smallest-divisor 1999)
;; 1999
;; (smallest-divisor 19999)
;; 7

;; ex 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-prime (- (runtime) 
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes low high)
  (cond ((< low high)
         (timed-prime-test low)
         (search-for-primes (+ low 2) high))))

;; smallest primes > 1000 : 1009, 1013, 1019 time: 3~4 seconds
;; smallest primes > 10000 : 10007, 10009, 10037 time: 8~9 seconds
;; smallest primes > 100000 : 100003, 100019, 100043 time: 28~36 seconds

;; ex 1.23
(define (next x)
  (cond ((= x 2) 3)
        (else (+ x 2))))
;; even though the number of steps is cut in half, the length of process execution
;; could depend more on "start up" time than anything
;; the next function also adds an aditional control structure to be parsed / evaluated rather than just doing + 1

;; ex 1.24
;; fermat test seems roughly O(log n) in practice
;; ex 1.25
;; the new expmod procedure will result in the same answer, but because the remainder
;; isn't being calculated along the way, we eventually end up doing arithmetic on really large numbers
;; slowing down then execution

;; doing incremental modolus prevent overflow issues / slowing down because the numbers being
;; used always remain < m

;; ex 1.26
;; by using two recursive calls instead of squaring one result, the number of calls now
;; grows linearly with the size of the exponent
;; (this is caused by tree-recursion instead of linear-recursion)
;; the depth of the tree with be log(n) , so the total number of nodes will be 2^log(n) = O(n)

;; towards the end of this chapter the book feels much more like number theory than computer programs
;; I guess it's enlightening to know that all of these processes can be formalized using a simple
;; Lisp language. It's not that bad if you're just reading and not doing the exercises. But the exercises
;; do assume you kinda know what you're doing number theory wise esp. with primes. Feels like implementing
;; Machine Learning papers
