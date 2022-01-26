#lang sicp

;; Multiple Representations for Abstract Data

;; abstraction barriers isolate implementation from procedure
;; powerful tool for controlling complexity

;; doesn't always make sense to speak of the 'underlying representation'
;; - more than one useful representation
;; - might want to support multiple
;; - in a changing system, data representation needs to be flexible
;; - want to additively incorporate new code

;; solution: generic procedures & type tagging

;; complex number arithmetic
;; possible representations:
;; Coordinates // Polar form

(define (add-complex z1 z2)
  (make-from-real-imag
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang
   (* (magnitude z1) (magnitude z2))
   (* (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))


;; using real / imag representation:
;; use suffix 'r'

(define (square x) (* x x))

(define (real-part-r z) (car z))
(define (imag-part-r z) (cdr z))

(define (magnitude-r z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle-r z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag-r x y)
  (cons x y))

(define (make-from-mag-ang-r r a)
  (cons (* r (cos a)) (* r (sin a))))

;; magnitude // angle representation (Polar)
;; use suffix 'p'

(define (real-part-p z)
  (* (magnitude-p z) 
     (cos (angle-p z))))

(define (imag-part-p z)
  (* (magnitude-p z) 
     (sin (angle-p z))))

(define (magnitude-p z) (car z))
(define (angle-p z) (cdr z))

(define (make-from-real-imag-p x y)
  (attach-tag 
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))

(define (make-from-mag-ang-p r a)
  (attach-tag 'polar (cons r a)))

;; implement type-tagging system

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; type-checking
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; generic selectors:
;; strategy: dispatch on type
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-r (contents z)))
        ((polar? z)
         (real-part-p (contents z)))
        (else (error "Unknown type: 
               REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-r (contents z)))
        ((polar? z)
         (imag-part-p (contents z)))
        (else (error "Unknown type: 
               IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-r (contents z)))
        ((polar? z)
         (magnitude-p (contents z)))
        (else (error "Unknown type: 
               MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-r (contents z)))
        ((polar? z)
         (angle-p (contents z)))
        (else (error "Unknown type: 
               ANGLE" z))))

;; generic constructors
(define (make-from-real-imag x y)
  (make-from-real-imag-r x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-p r a))


;; representing tables (as seen in 3.3.3)
;; required to use a type/operation table for data-directed programs

;; lookup checks if there's a record in the table associated
;; with the given key
;; if there is, then return the value of the record
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

;; assoc iterates the table and returns the record with the given
;; key if it exists
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))

;; if there is already a record in the table with the given
;; key, replace the current value with the given value.
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; multi dimensions:
;; first key identifies a subtable
;; secnd key identifies a record within the subtable

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record (cdr record) false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr!
               subtable
               (cons (cons key-2 value)
                     (cdr subtable)))))
        (set-cdr!
         table
         (cons (list key-1 (cons key-2 value))
               (cdr table)))))
  'ok)


;; apply a procedure given a generic operation + arguments
"
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           No method for these types: APPLY-GENERIC
           (list op type-tags))))))
"