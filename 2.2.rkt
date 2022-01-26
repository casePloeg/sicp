#lang sicp
;; Hierarchical Data and the Closure Property

;; Closure -- refers to a "closed operation" on a set
;; closure property of cons, we can make pairs of pair objects
;; -- supports hierarchical structures
;; closure can also refer to a programming language property of scoping free variables

;; sequences
;; ordered collection of data objects

;; list:
(cons 1
      (cons 2
            (cons 3
                  (cons 4 nil))))

(list 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))

(define (length items)
  (define (length-iter items count)
    (if (null? items)
        count
        (length-iter (cdr items) (+ count 1))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))
;; ex 2.17
;; cute
(define (last-pair items)
  (cons (list-ref items (- (length items) 1)) nil))

;; ex 2.18
;; how to do this "recursively" ? - have to use append because of the way cons is designed,
;; the first param needs to be the atom

(define (reverse items)
  (define (reverse-iter items rl)
    (if (null? items)
        rl
        (reverse-iter (cdr items) (cons (car items) rl))))
  (reverse-iter items (list)))

;; ex 2.19
;; update change counting problem 1.2.2 to use lists for currency
;; ordering will not matter, we are counting the combinations, whether you
;; start with pennies or quarters will have no effect when using both
(define us-coins
  (reverse (list 50 25 10 5 1)))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination l)
  (car l))

(define (except-first-denomination l)
  (cdr l))

(define no-more? null?)

(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         ;; separate into two cases:
         ;; 1. all combos that do not use the first coin
         ;; 2. all combos that must use first coin
         (+ (cc
             amount
             (except-first-denomination
              coin-values))
            (cc
             (- amount
                (first-denomination
                 coin-values))
             coin-values)))))

(cc 100 us-coins)

;; ex 2.20
;; dotted-tail notation:
;; if prepended by a dot,
;; let the last param represent a list of remaining args

(define (f x y . z) 0)
;; (define f (lambda (x y . z) 0))

(define (same-parity z . nums)
  (define (iter nums)
    (if (null? nums)
        '()
        (if (= (modulo z 2) (modulo (car nums) 2))
            (cons (car nums) (iter (cdr nums)))
            (iter (cdr nums)))))
  (iter (cons z nums)))

;; maps (transformation lists)

"
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))
"

"
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
"
;; defining scale-list using map makes us think about the procedure differntly
;; scaling *is* mapping
(define (scale-list items  factor)
  (map (lambda (x) (* x factor))
       items))

;; ex 2.21
"
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))
"
(define (square-list items)
  (map (lambda (x) (* x x)) items))
(square-list (list 1 2 3 4))

;; ex 2.22
;; iterative will append backwards because we start with nil and append
;; flipping arguments doesn't work because cons requires the atom as first param

;; ex 2.23
;; implement for-each

;; this works, just ignore the returned values because
;; all we care about is applying the procedure
(define for-each map)

;; 2.2.2 Hierarchical Structures

;; (define x (cons (list 1 2) (list 3 4)))
;; (length x)
;; (count-leaves x)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(list 1 (list 2 (list 3 4)))
"
1
|2
 |3
  4
1 -> 2 -> 3, 4
(1 (2 (3 4)))
"
;; ex 2.25

;; (1 3 (5 7) 9) (cdr (car (cdr (cdr x))))
;; ((7)) (car (car x)) 
;; (1 (2 (3 (4 (5 (6 7)))))) (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))


;; ex 2.26
;; (define x (list 1 2 3))
;; (define y (list 4 5 6))

;; (append x y) --> (1 2 3 4 5 6)
;; (cons x y)   --> ((1 2 3) 4 5 6)
;; (list x y)   --> ((1 2 3) (4 5 6))


;; ex 2.27
"
(define (reverse items)
  (define (reverse-iter items rl)
    (if (null? items)
        rl
        (reverse-iter (cdr items) (cons (car items) rl))))
  (reverse-iter items (list)))
"
(define (deep-reverse items)
  (define (reverse-iter items rl)
    (cond ((null? items) rl)
          ((not (pair? items)) items)
          (else (reverse-iter
                 (cdr items)
                 (cons (reverse-iter (car items) '()) rl)))))
  (reverse-iter items (list)))

(define (deep2 t)
  (if (pair? t)
      (reverse (map deep-reverse t))
      t))

;; ex 2.28
(define x 
  (list (list 1 2) (list 3 4)))
(define (fringe x)
  (define (iter x leaves)
    (cond ((null? x) leaves)
          ((pair? x) (append (iter (car x) leaves) (iter (cdr x) leaves)))
          (else (list x))))
  (iter x '()))

;; ex 2.29

;; binary mobile - two branches
(define (make-mobile left right)
  (list left right))

;; structure can be number or another mobile
(define (make-branch length structure)
  (list length structure))

;; selectors
(define left-branch car)
(define (right-branch x) (car (cdr x)))
(define branch-length car)
(define (branch-structure x) (car (cdr x)))


(define (total-weight-branch b)
    (let ((len (branch-length b))
          (struct (branch-structure b)))
      (if (pair? struct)
          (total-weight-mobile struct)
          struct)))

(define (total-weight-mobile x)
  (let ((lb (left-branch x))
        (rb (right-branch x)))
    (+ (total-weight-branch lb) (total-weight-branch rb))))

;; a mobile is balanced if:
;; the length of the left rod * weight from left rod = corresponding right rod
;; and if all submobiles are balanced
;; if a branch has a submobile instead of a weight, consider its weight to be zero? or balance against submobile total weight

;; using 'total-weight-mobile' inside of the torque procedure creates additional complexity O(n) per call
;; would be better to *map* over the mobile first to figure out the total weight for each submobile and
;; then have those values to reference instead of re-calculating each time
(define (balanced? m)
  (define (torque b)
    (let ((len (branch-length b))
          (struct (branch-structure b)))
      (if (pair? struct)
          (cons (* len (total-weight-mobile struct)) (balanced? struct))
          (cons (* len struct) #t))))
  (let ((lb (left-branch m))
        (rb (right-branch m)))
    (let ((tlb (torque lb))
          (rlb (torque rb)))
      (and (= (car tlb) (car rlb)) (cdr tlb) (cdr rlb)))))

(define z (make-mobile (make-branch 1 1) (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 1 4)))))
(define btree (make-mobile (make-branch 1 4) (make-branch 2 2)))

;; mapping over trees

"
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree factor))
        (else
         (cons (scale-tree (car tree)
                           factor)
               (scale-tree (cdr tree)
                           factor)))))
"

;; tree is a sequence of subtrees
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; ex 2.30
"
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))
"
"
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (* tree tree))))
"
;; ex 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

;; ex 2.32
;; set: (1 2 3)
;; all subsets: (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;; 1, (2 3)
;; all subsets: (() (3) (2) (2 3))
;; append 1 to all subsets to get the original answer
;; 2, (3)
;; (() (3))
;; 3
;; (())
;; the pattern in the data makes this easier to see that is otherwise might have been
;; organizing data can be useful

;; "combinations"
;; similar to coin-counting from 1.2.2 :)
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; generate all subsets for the given set -1 item
;; all subsets involving that item will be all prev. subsets with the item appended
;; use map to add item to "rest" of subsets

;; 2.2.3 Sequences as Conventional Interfaces
(define (square x) (* x x))

"
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree)) (sum-odd-squares (cdr tree))))))
"
(define (fib k)
  (cond ((= 0 k) 1)
        ((= 1 k ) 1)
        (else (+ (fib (- k 1)) (fib (- k 2))))))
"
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
"
;; enumerate - there is no general enumeration, they are context specifics
;; filter
;; map
;; accumulate

;; if we can organize programs to make the signal-flow structure manifest in the procedures we write,
;; this would increase the conceptual clarity of the resulting code.

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate
   +
   0
   (map square
        (filter odd?
                (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even?
           (map fib
                (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square
           (map fib
                (enumerate-interval 0 n)))))

(define
  (product-of-squares-of-odd-elements
   sequence)
  (accumulate
   *
   1
   (map square
        (filter odd? sequence))))

;;example
"
(define
  (salary-of-highest-paid-progammer
   records)
  (accumulate
   max
   0
   (map salary
        (filter programmer? records))))
"
;; (accumulate max 0) will give us the maximum value in the sequence (or zero), pretty cool

;; in 3.5 we generalize sequences to allow infinite streams :)

;; ex 2.33
(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length2 sequence)
     (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; ex 2.34
;; Horner's rule for polynomial evaluation as an accumulation
(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

;; ex 2.35
(define (count-leaves2 t)
  (accumulate (lambda (x y) (+ 1 y)) 0 (enumerate-tree t)))

;; ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (accumulate (lambda (seq rest) (cons (car seq) rest)) nil seqs))
            (accumulate-n op init (accumulate (lambda (seq rest) (cons (cdr seq) rest)) nil seqs)))))

;; ex 2.37
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

;; implement matrix and vectors operations using the sequence interface
;; having done this in haskell before, this is always annoying
;; dot-product
;; matrix-*-vector
;; matrix-*-matrix
;; transpose

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (vector-*-matrix v m)
  (map (lambda (x) (dot-product x v)) (transpose m)))

(define (transpose mat)
  (accumulate-n
   cons
   nil
   mat))

;; (mn)_i = m_i * nT_i
;; this definition will allow matrix multiplication with invalid sizes
;; for example M * M will be computed even if M is not a square matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (row) (matrix-*-vector cols row))
     m)))


;; ex 2.38 (returning to this book after a few weeks break)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;; (fold-right / 1 (list 1 2 3)) -> (3 / (2 / (1 / 1))) -> 3/2 (head is the first arg)
;; (fold-left / 1 (list 1 2 3)) -> (((1 / 3) / 2 ) / 1) -> 1/6 (head is the second arg)

;; to get the same values op should be "associative" (op a (op b c)) = (op (op a b) c)
;; folding from left or right is an example of "re-grouping" the elements using the parenthesis
;; if the elements were being "re-ordered", like (5 + 3 + 4) = (4 + 5 + 3), then it would require
;; the "communicative" property

;; ex 2.39
"
(define (reverse sequence)
  (fold-right
   (lambda (x y) (append y (list x))) nil sequence))
"
(define (reverse2 sequence)
  (fold-left
   (lambda (x y) (cons y x)) nil sequence))

;; NESTED MAPPINGS
"
(accumulate
 append
 nil
 (map (lambda (i)
        (map (lambda (j)
               (list i j))
             (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))
"

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (inc test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;; the combination of mapping + accumulating with append is popular
;; take a sequence -> turn each element into a list -> append each list into a bigger list (flatten the lists)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; predicate
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; generate the results (triple)
(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))


;; generate permutations of S
;; for each x, append x to each element in perms(S - x)
(define (permutations s)
  (define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutations
                       (remove x s))))
               s)))

;; ex 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
                   
           (enumerate-interval 1 n)))

;; ex 2.41
;; using an absolutely janky lambda function to filter instead of generating the ordered triplets directly
(define (triple-sum n s)
  (map (lambda (pair) (cons (- s (car pair) (cadr pair)) pair))
       (filter
        (lambda (pair)
          (and (> (- s (+ (car pair) (cadr pair))) 0)
               (>= n (- s (+ (car pair) (cadr pair))))
               (not (= (- s (+ (car pair) (cadr pair))) (car pair)))
               (not (= (- s (+ (car pair) (cadr pair))) (cadr pair)))
               (> (- s (+ (car pair) (cadr pair))) (car pair))))
        (unique-pairs n))))


(define empty-board nil)
(define (adjoin-position row col board)
  (cons (list row col) board))

(define (safe? col board)
  (define (remove queen board)
    (filter (lambda (q2) (not (and (= (car queen) (car q2)) (= (cadr queen) (cadr q2))))) board))
  (let ((q1 (car (filter (lambda (pos) (= col (cadr pos))) board))))
    (accumulate
     (lambda (x y) (and x y))
     #t
     (map (lambda (q2)
           (not (or
            (= (car q1) (car q2)) ;; same row
            (= (cadr q1) (cadr q2)) ;; same col
            (= (abs (- (car q1) (car q2))) (abs (- (cadr q1) (cadr q2))))))) ;; same diagonal
         (remove q1 board)))))


;; ex 2.42 8 queens problem
(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queens-cols (- k 1))))))
  (queens-cols board-size))

;; ex 2.43
;; complexity analysis
;; swapping the rest-of-queens with new-row
;; results in solving (queen-cols (- k 1)) board_size times instead of just once
;; this happens at every level, so the complexity cost is exponential (tree like structure)
;; ~~~ N^N * T1



;; A Picture Language!

;; build arbitrarily complex patterns
;; focus on primitives, combination, and means of abstraction

;; stratified design:
;; a useful language will have multiple layers where each layer in built on the previous layer's
;; primitives, combinations, and methods of abstraction

;; think: transitors & resistors --> logic gates --> computer architecture

;; closure: (in the Lisp sense), a technique usedd for representing procedures with free variables
