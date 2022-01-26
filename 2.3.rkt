#lang sicp
;; 2.3 Symbolic Data

;; quotation:
;; adding the ability to quote literal strings is powerful, but also messy
;; rules for equality are much harder for strings than values
;; quotation enables *interpretation*

;; use single quote to treat lists as data instead of as an expression

(define a 1)
(define b 2)

(list a b)
(list 'a 'b)
(list 'a b)

(car '(a b c))
(cdr '(a b c))
'() ;; replace the variable: nil  by quoting the empty list
;; this is the same as having a quote procedure (quote (list a b c))
;; any expression seen by the interpreter can be manipulated a data object


;; eq? --> check if two symbols are the 'same'; if they consist of the same characters in the same order

;; return #f if item is not in x
;; otherwise,
;; return the sublist with item at the head 
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; ex 2.53
(list 'a 'b 'c) ;; (a b c)
(list (list 'george)) ;; ((george))
(cdr '((x1 x2) (y1 y2))) ;; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;; (y1 y2)
(pair? (car '(a short list))) ;; #f
(memq 'red '((red shoes) (blue socks))) ;; #f
(memq 'red '(red shoes blue socks)) ;; (red shoes blue socks)

;; ex 2.54

(define (equal? a b)
  (cond
    ((null? a) (null? b))
    ((and (list? a) (list? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
    (else (eq? a b))))

;; ex 2.55
(car ''abracadabra);; #quote
;; this is the same as:
(car '(quote abracadabra))
(car (quote (quote abracadabra)))
;; since lists and expressions and the same thing,
;; car will give the first element in the list, which is the quote procedure
;; SHEESh

;; 2.3.2 Example: Symbolic Differentiation
;; (variable? e) ;; is e a variable?
;; (same-variable? v1 v2) ;; are v1 and v2 the same variable?
;; (sum? e) ;; is e a sum?
;; (addend e) ;; first part of a sum
;; (augend e) ;; second part of a sum
;; (make-sum a1 a2) ;; construct sum of a1 and a2
;; (product? e) ;; is e a product?
;; (multiplier e) ;; first part of product
;; (multiplicand e) ;; second part of product
;; (make-product m1 m2) ;; construct the product of m1 and m2

;; don't change this procedure, it's beautiful
(define (deriv exp var)
  (display exp)
  (newline)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
            (multiplier exp)
            (deriv (multiplicand exp) var))
          (make-product
            (deriv (multiplier exp) var)
            (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))


(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;; change these constructors to simplify expressions!
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

;; will always run first, look for a + sign anywhere in the chain
(define (sum? x)
  (and (pair? x) (or (eq? (car x) '+) (sum? (cdr x)))))

;; check for products after sums
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

;; to split a summation, go left to right until a + sign is found
;; special case of (x + (...)) , make sure to return x as a variable instead
;; of as a list
(define (addend s)
  (define (iter exp s)
    ;; (display s)
    (cond
      ((and (eq? (car s) '+) (null? (cdr exp))) (car exp)) 
      ((eq? (car s) '+) exp)
      (else (iter (cons (car s) exp) (cdr s)))))
  (iter '() s))

(define (augend s)
  (define (iter exp s)
    ;; (display s)
    (cond
      ((and (eq? (car s) '+) (null? (cddr s))) (cadr s)) 
      ((eq? (car s) '+) (cdr s))
      (else (iter (cons (car s) exp) (cdr s)))))
  (iter '() s))


(define (multiplier p) (car p))
(define (multiplicand p)
  (cond ((null? (cddr p)) 1)
        ((null? (cdddr p)) (caddr p))
        (else (cddr p))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

;; ex 2.56
;; implement the power rule

;; ex 2.57
;; extend differentiation to handle sum / products of more than two numbers

;; ex 2.58
;; reimplement constructors, predicates, and selectors to accomodate infix notation and ordering of operations

;; Example: Representing Sets
;; unlike rational numbers and algebraic expressions, the choice for representation in terms of lists
;; is not as straightforward for sets.

;; sets: collection of distinct objects
;; defined by specifying the operations that can be used on a set
;; union
;; intersection
;; element-of?
;; adjoin (add)

;; data-abstraction: we are free to implement sets however we please given that they implement the above operations
;; ensuring consistency is usually done by following well-defined rules (mathematical constraints and invariants)

;; SETS as unordered lists
"
;; empty set is empty list
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; only add x to the set if x is not already in the set
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

;; ex 2.59 implement union-set
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1)
                    set2))
        (else (cons (car set1)
                    (union-set (cdr set1)
                               set2)))))

;; ex 2.60
;; if we allow duplicates:
;; union can now be defined in O(N) time instead of O(N^2)
;; adjoin can now be defined in O(1) time instead of O(N)

;; element-of? will still be O(N) , but this N was grow much faster with usage of union/adjoin
;; intersection will still be O(N^2), but N can be much bigger long term

;; this implementation will be good if we need to do a lot of unions in our work
;; is the data typically have little amounts of natural duplicates, it would
;; probably work well to allow them, since the size of our sets would be much affected by the time complexity is lowered
;; by taking away the no-duplicate constraint
"
;; SETS as ordered lists

;; establish an ordering of the elements that will be stored in the set

;; now we do not have to scan the whole set to check containment
(define (element-of? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of? x (cdr set)))))
;; in the worst case (looking for the largest element), this saves no steps
;; on average we expect to search half the list, which is still O(N) growth
;; but does save time by a factor of (2?)

;; intersection gets a more impressive speed up
;; on each iteration, we can decrease the size of one or both of the sets we are operating on
;; so this procedure will run in O(N) time

(define (intersection set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection
                          (cdr set1)
                          set2))
              ((< x2 x1) (intersection
                          set1
                          (cdr set2)))))))

;; ex 2.61
;; implement adjoin using the ordered list representation
(define (adjoin set x)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin (cdr set) x)))))

;; ex 2.62
;; implement union using the ordered list representation
(define (union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union (cdr set1) set2)))
                 ((< x2 x1) (cons x2 (union set1 (cdr set2)))))))))



;; SETS as binary trees
;; a node in the tree has two children
;; each node holds a number
;; left child < parent
;; right child > parent

;; multiple trees are possible to represent a set

;; advantage: search is reduced to O(logN) time -- halve the size of the problem at each step
"
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-? x (left-branch set)))
        ((> x (entry set)) (element-of-? x (right-branch set)))))

(define (make-leaf x)
    (make-tree x '() '()))

(define (adjoin- x set)
  (cond ((null? set) (make-leaf x))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

;; the above will have O(logN) time complexity if the trees are 'balanced'
;; we can deleveop a balancing algorithm
;; we can design new data structures like B-trees or Red-Black trees, AVL trees, etc.

;; ex 2.63
;; two procedures that convert (flatten) binary trees to a list
;; are there differences in the lists that are produced?
;; do they have the same time complexity?


;; at each level, left tree list + element + right tree list, using append
;; O(N * logN) time complexity because each call to append is O(N) and the input is split on each level
;; --> there will be logN levels in the tree, so O(N) per level * O(logN) levels
;; T(N) = 2 * T(n/2) + O(N)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1
              (right-branch tree))))))

;; at each level, at one element to the list,
;; defer appending to the recursive stack
;; O(N) time complexity , if we assume copying the data of parameter result-list is done in constant time (not linear)
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))


;; tree->list-1 
;; tree->list-2
;; both procedures make a pre-order traversal of the tree, (left + center + right)

(define t (make-tree 5 (make-tree 4 (make-leaf 2) (make-leaf 3)) (make-tree 6 (make-leaf 7) (make-leaf 8))))

;; ex 2.64
;; convert an ordered list to a balanced binary tree
(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

;; it would be nice if within the scope of a let block,
;; sequential expressions could access previously defined variables
;; instead of having to create another let block
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
;; how does partial tree work
(define l (list 1 3 5 7 9 11))
;; step 1: we want a tree with n elements
;; step 2: the size of the left tree is (n-1)/2 , (take n, remove 1 for the root node, and split in half)
;; step 3: construct a balanced tree of size (n-1)/2 using the smallest elements in the list
;; step 4: of the elements left over, the first in the ordering will be the root
;; step 5: the rest of the elements comprise of the right side of the tree, all are larger than the root,
;; size of the right tree is (n - (left-size + 1)) , +1 for the root element
;; step 6: construct the right sided tree
;; step 7: combine left + root + right
;; will take: O(N) time, T(N) = 2 * T(N/2) + 1
;; in other words, for each node in the tree that's produced, only constant work is done, there will be O(N) nodes

;; ex 2.65
;; write O(N) implementations of union-set and intersection-set for sets implemented as (balanced) binary trees
;; use list->tree and tree-> list

;; union:
;; make both trees into ordered lists
;; use union procedure for ordered lists, O(N) time, to merge lists together
;; make tree out of merged list

;; we use two different representations to optimize different operations
;; ordered lists can do fast union/intersection: O(N)
;; balanced trees can do fast insert/search: O(logN)
(define (union-tree set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (let ((union-list (union list1 list2)))
      (list->tree union-list))))

(define (intersection-tree set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (let ((intersection-list (intersection list1 list2)))
      (list->tree intersection-list))))

;; implement a 'lookup' procedure for binary-search-tree representation of a set
;; (make a slight modification to the element-of procedure)

(define key car)

(define (lookup given-key set)
  (cond ((null? set) false)
        ((= given-key (key (entry set))) (entry set))
        ((< given-key (key (entry set))) (lookup given-key (left-branch set)))
        ((> given-key (key (entry set))) (lookup given-key (right-branch set)))))

"
;; EXAMPLE: Huffman Encoding Trees
;; we want to store data as a series of ones and zeros (bits)
;; ASCII standard code represents text by encoding each character as a sequence of seven bits.
;; 2^7 = 128 = all possible different characters
;; n distinct characters requires log_2(n) bits per symbol, because with log_2(n) bits there are 2^(log_2(n)) possible characters = n

;; if we have characters A-H, we could encode them as:
;; A: 000
;; B: 001
;; C: 010
;; D: 011
;; E: 100
;; F: 101
;; G: 110
;; H: 111

;; This is known as a 'fixed-length' code, since each symbol is encoded with the same amount of bits

;; Sometimes it works better to have a 'variable-length' code, where differeny symbols have different lengths
;; Morse code: could not use the same number of dots + dashes for each letter
;; 'E' , the most frequent letter, is represented as a single dot.
;; In general, if you can encode high-frequency symbols with less bits than the average,
;; you can save space encoding the message

;; Consider this alternative code:
;; A: 0
;; B: 100
;; C: 1010
;; D: 1011
;; E: 1100
;; F: 1101
;; G: 1110
;; H: 1111

;; to encode the message "BACADAEAFABBAAAGAH",
;; the first code requires 54 bits
;; the second code only requires 42 bits (save > 20% space)

;; One issue of variable-length codes is knowing when to stop reading a symbol and start the next
;; Morse Code solves this by using a special 'separator code' , which mades Morse Code actually Ternary rather than Binary

;; Another option is to design the code so no complete code for any symbol is a prefix for another.
; Since A: 0 and B: 100 , no other code can start with either 0 or 100
;; this way, we know that if we've read in a full character, we know to go onto the next one with no chance of ambiguity

;; using variable-length, prefix codes while considering relative frequences of characters will usually attain significant savings
;; and can be proven to always have a net-positive effect-(optimal)-using Huffman Encoding (discovered by David Huffman)

;; huffman tree is a binary tree with symbols @ leaf nodes
;; non-leaf nodes represent sets of symbols, and have combined weights based on symbol frequency in a text (used for construction)

;; given a tree, find the encoding by starting at the root, go left = add a 0 to the code, go right = add a 1
;; to decode, follow the 0's and 1's to get to the symbol then go back to the root and repeat

;; generating huffman trees:
;; given an alphabet and relative frequencies
;; how to encode with the fewest bits? -> follow huffman algorithm -> can be proven optimal

;; arrange the tree so symbols with lowest freq appear furthest from the root

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)
                    (cadr pair))
         (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))
(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree) ;; A D A B B C A

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

;; ex 2.68
(define (element-of-set? x set)
  (cond ((null? set) false)
        (else (or (equal? x (car set)) (element-of-set? x (cdr set))))))

(define (encode-symbol s tree)
  (cond ((and (leaf? tree) (equal? (symbol-leaf tree) s)) '())
        ((element-of-set? s (symbols (left-branch tree))) (cons 0 (encode-symbol s (left-branch tree))))
        ((element-of-set? s (symbols (right-branch tree))) (cons 1 (encode-symbol s (right-branch tree))))
        (else (error "bad symbol: encode-symbol" s))))

;; ex 2.69
;; we have an ordered set implementation so the trees
;; will the least weight will always be at the front
;; make a new, larger, subtree out of the two lightest
;; subtrees on each iteration.
(define (successive-merge leaf-set)
  (cond
    ((null? leaf-set) '())
    ((null? (cdr leaf-set)) (car leaf-set))
    (else (successive-merge (adjoin-set
                            (make-code-tree (car leaf-set)
                                            (cadr leaf-set))
                            (cddr leaf-set))))))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

;; ex 2.70
(define rocktree (generate-huffman-tree '((A 2) (NA 16) (BOOM  1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))) 

(define rock-song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)) 
(length (encode rock-song rocktree))
(* 3 (length rock-song))

;; ex 2.71
;; if the relative frequencies are 1, 2, 4, ... 2^n-1
;; then subtree created by merging the two lightest subtrees
;; will always remain as the lighest subtree remaining
;; thus, the tree will look closer to a linked list than
;; a balanced tree
;; most frequent symbol -> 1 bit
;; least frequent symbol -> n-1 bits

;; ex 2.72
;; steps required to encode a symbol
;; requires walking down the huffman tree which can be O(N) height
;; each step on the walk requires searching the element-set,
;; which can be O(N)
;; a rough worst case would be O(N^2), though if you need to
;; walk further and further down the tree, the frequency of
;; the symbol must be low, which means the time required to
;; search must have been low to start. so maybe the time
;; complexity is more like O(N^1.5)

;; the steps to encode bot hthe most frequent and least frequent
;; symbols in the example scenario from ex 2.71 will be O(N)
;; most freq: find the correct leaf node immediately with O(N) search
;; least freq: traverse to the bottom of the tree of O(N) height,
;;             each search takes O(1) because the symbol is least
;;             freq and will always be at the start of the set
