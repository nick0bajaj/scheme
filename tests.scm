;;; Test cases for Scheme.
;;;
;;; In order to run only a prefix of these examples, add the line
;;;
;;; (exit)
;;;
;;; after the last test you wish to run.

;;; **********************************

(car (cdr '(90 . ((45 . ((7)))))))
; expect (45 (7))

(cdr (cdr '(25 . ((56 . ((97)))))))
; expect ()

(cons 5 (cons 7 (cons 9 '(love_you))))
; expect (5 7 9 love_you)

(equal? '(4 . ((5 . 6))) (cons 4 (cons (cons 5 6) nil)))
; expect True

(define (filter f lst)
  (cond ((null? lst) '())
    ((f (car lst)) (cons (car lst) (filter f (cdr lst))))
    (else (filter f (cdr lst))))
)
; expect filter

(filter even? '(0 5 5 5 5 5 9 8 8 10))
; expect (0 8 8 10)

+
; expect #[+]

exit
; expect #[exit] 

=
; expect #[=]

(+ 1 4 5 79 8 7 6 (* 84 7 (+ 2 4)) (- 56 9 10 (* 6 9 8)))
; expect 3243

(* 89 87 (- 56 7 6 5 (+ 6 7 9)) (+ 89 4 4 (/ 9768 56)) 90 45)
; expect 136188308571.42857

(/ 8965 (+ 1000 5 6 4) (- 56 10 12 (* 9 2)) 12)
; expect 0.04600266830870279

(define a 45)
; expect a

(define b (/ 90 a))
; expect b

a
; expect 45

(- a (* b a) 35 (/ (* 8 b) 2))
; expect -88


(eval (define my_project (* 56.8976 56 7 8 7)))
; expect 1249016.1152

(eval (define cool ( + (* 9 98 7 6) (- 56 7 8 2 3) 67 56)))
; expect 37203

'(hi my name is ralph)
; expect (hi my name is ralph)

(eval (cons 'car '('(1 2 (45 (56))))))
; expect 1

'(1 5 '(7 . 8) '(89 89 76 . 6) (2 three . (4 . 5)))
; expect (1 5 (quote (7 . 8)) (quote (89 89 76 . 6)) (2 three 4 . 5))

(begin (* 87 6 5 4 3) (+ 2 3) (define x (+ 67 9)) (define y (* 7 6 5)))
; expect y

(define t (begin (display 52) (* 6 8 9)))
; expect 52t

(begin '(+ 2 3) (* 8 9 8) eval(cdr '(1 2 3)) '(1 4 5))
; expect (1 4 5)

(lambda (x y z d) (+ x y (/ z d) (* x y d)))
; expect (lambda (x y z d) (+ x y (/ z d) (* x y d)))

(define (make-devalued num)
  (lambda (x) (- x num (/ num x))))
; expect make-devalued

(define minimal_decrease (make-devalued 100))
; expect minimal_decrease

(minimal_decrease 8)
; expect -104.5

(define (composed f g)
  (lambda (x) (f (g x)))
)
; expect composed

((composed abs abs) -5)
; expect 5

(define (triangle x y z) (* 8 x y z))
; expect triangle

triangle
; expect (lambda (x y z) (* 8 x y z))

filter
; expect (lambda (f lst) (cond ((null? lst) (quote ())) ((f (car lst)) (cons (car lst) (filter f (cdr lst)))) (else (filter f (cdr lst)))))

(define (remove item lst)
  (cond ((null? lst) '())
        ((equal? item (car lst)) (remove item (cdr lst)))
        (else (cons (car lst) (remove item (cdr lst)))))
)
; expect remove

remove
; expect (lambda (item lst) (cond ((null? lst) (quote ())) ((equal? item (car lst)) (remove item (cdr lst))) (else (cons (car lst) (remove item (cdr lst))))))

(define (substitute s old new)
  (cond ((null? s) nil)
        ((pair? (car s)) (cons (substitute (car s) old new)
                               (substitute (cdr s) old new)))
        ((equal? (car s) old) (cons new
                               (substitute (cdr s) old new)))
        (else (cons (car s) (substitute (cdr s) old new))))
)
; expect substitute

substitute
; expect (lambda (s old new) (cond ((null? s) ()) ((pair? (car s)) (cons (substitute (car s) old new) (substitute (cdr s) old new))) ((equal? (car s) old) (cons new (substitute (cdr s) old new))) (else (cons (car s) (substitute (cdr s) old new)))))

(substitute '(c d m y y) 'y 's)
; expect (c d m s s)

(remove 8 '(8 8 8 8 76 5 4 3 8 9 1))
; expect (76 5 4 3 9 1)

(remove 10 nil)
;expect ()

(define (ordered? s)
  (if (or (null? s) (null? (cdr s)))
    true
    (and (<= (car s) (car (cdr s))) (ordered? (cdr s)))))
; expect ordered?

(ordered? '(1 4 5 9 4 5 3 2))
; expect False

(and 68 97 56 90 (* 8 6 7 (- 5 2) 5) False (/ 1 0) False)
; expect False

(and (/ 1 0) False 34 56)
; expect Error: cannot divide by zero

(and 70 60 87 (and 45 67 False) 30 90)
; expect False

(or False False False (* 5 6 (- 67 5)) True)
; expect 1860

(or False (or))
; expect False

(or False (or False False 56) 45 67)
; expect 56

(cond ((= 7 8) 'pup)
  ((< 9 6) 'lovecs)
  ((> 10 6) (* 8 9 (- 89 7)))
  (else (* 6 7)))
; expect 5904

(cond ((= 9 10) 'love)
  ((> (* 2 2) 6) 'berkeley)
  ((= 10 15) (+ 5 4))
  (else 'forever))
; expect forever

(cond ((= 67 10) 'cool)
  ((> (* 2 8) 6))
  ((= 17 15) (+ 5 4))
  ((= 9 9) 'gato)
  ((= 55 55) 'melon)
  (else 'hoorah))
; expect True

(define x 19)
; expect x

(define y 20)
; expect y

(let ((x (* 87 6))
     (y (* (* x 9 8) 8)))
    (list x y))
; expect (522 10944)

(define z 30)
; expect z

(define a 100)
; expect a

(let ((x (* 7 x))
      (y (* y x z a))
      (z (/ a y)))
      (list x y z))
; expect (133 1140000 5)

(define my_mu (mu (a b c) (* a b (+ b c) (* a 5))))
; expect my_mu

(define my_lambda (lambda (a b c d e) (my_mu (* a e (/ d b (- c a (+ e b)))) (* a b) (/ c d (+ a e)))))
; expect my_lambda

(my_lambda 78 68 50 4 3)
; expect 2719275.1486342936

(define my_mu_two (mu (a b) (* 55 (+ 2 a) (/ 65 b))))
; expect my_mu_two

(define my_lambda_two (lambda (x y) (my_mu_two (* x y) (+ x (/ y x) y 45))))
; expect my_lambda_two

(my_lambda_two 58 97)
; expect 99766.24775583483

(define (must_add_to lst x)
  (cond ((null? lst) (cons x nil))
        (else (cons x lst))))
; expect must_add_to

(must_add_to (list 12 3 4) 10)
; expect (10 12 3 4)

(define cool_lambda (lambda (x y z) (+ 45 x y (* 2 z))))
; expect cool_lambda

(cool_lambda 5 6 10)
; expect 76

(define empty_lambda (lambda () (* 45 6)))
; expect empty_lambda

(empty_lambda)
; expect 270


;;; **********************************

;;; These are examples from several sections of "The Structure
;;; and Interpretation of Computer Programs" by Abelson and Sussman.

;;; License: Creative Commons share alike with attribution

;;; 1.1.1

10
; expect 10

(+ 137 349)
; expect 486

(- 1000 334)
; expect 666

(* 5 99)
; expect 495

(/ 10 5)
; expect 2

(+ 2.7 10)
; expect 12.7

(+ 21 35 12 7)
; expect 75

(* 25 4 12)
; expect 1200

(+ (* 3 5) (- 10 6))
; expect 19

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
; expect 57

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
; expect 57


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Move the following (exit) line to run additional tests. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; 1.1.2

(define size 2)
; expect size
size
; expect 2

(* 5 size)
; expect 10

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
; expect 314.159

(define circumference (* 2 pi radius))
circumference
; expect 62.8318

;;; 1.1.4

(define (square x) (* x x))
; expect square
(square 21)
; expect 441

(define square (lambda (x) (* x x))) ; See Section 1.3.2
(square 21)
; expect 441

(square (+ 2 5))
; expect 49

(square (square 3))
; expect 81

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)
; expect 25

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)
; expect 136

;;; 1.1.6

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(abs -3)
; expect 3

(abs 0)
; expect 0

(abs 3)
; expect 3

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 3 -2)
; expect 5

;;; 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
; expect 3.00009155413138

(sqrt (+ 100 37))
; expect 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
; expect 1.7739279023207892

(square (sqrt 1000))
; expect 1000.000369924366

;;; 1.1.8

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(sqrt 9)
; expect 3.00009155413138

(sqrt (+ 100 37))
; expect 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
; expect 1.7739279023207892

(square (sqrt 1000))
; expect 1000.000369924366

;;; 1.3.1

(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)
; expect 3025

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 1 10)
; expect 55

;;; 1.3.2

((lambda (x y z) (+ x y (square z))) 1 2 3)
; expect 12

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(f 3 4)
; expect 456

(define x 5)
(+ (let ((x 3))
     (+ x (* x 10)))
   x)
; expect 38

(let ((x 3)
      (y (+ x 2)))
  (* x y))
; expect 21

;;; 2.1.1

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

(define x (cons 1 2))
(car x)
; expect 1

(cdr x)
; expect 2

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
; expect 1

(car (cdr z))
; expect 3

z
; expect ((1 . 2) 3 . 4)

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))
(define one-half (make-rat 1 2))
(print-rat one-half)
; expect 1/2

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
; expect 5/6

(print-rat (mul-rat one-half one-third))
; expect 1/6

(print-rat (add-rat one-third one-third))
; expect 6/9

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(print-rat (add-rat one-third one-third))
; expect 2/3

(define one-through-four (list 1 2 3 4))
one-through-four
; expect (1 2 3 4)

(car one-through-four)
; expect 1

(cdr one-through-four)
; expect (2 3 4)

(car (cdr one-through-four))
; expect 2

(cons 10 one-through-four)
; expect (10 1 2 3 4)

(cons 5 one-through-four)
; expect (5 1 2 3 4)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))
; expect (10 2.5 11.6 17)

(map (lambda (x) (* x x))
     (list 1 2 3 4))
; expect (1 4 9 16)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(scale-list (list 1 2 3 4 5) 10)
; expect (10 20 30 40 50)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
; expect 4

(count-leaves (list x x))
; expect 8

;;; 2.2.3

(define (odd? x) (= 1 (remainder x 2)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))
; expect (1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
; expect 15

(accumulate * 1 (list 1 2 3 4 5))
; expect 120

(accumulate cons nil (list 1 2 3 4 5))
; expect (1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
; expect (2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; expect (1 2 3 4 5)

;;; 2.3.1

(define a 1)

(define b 2)

(list a b)
; expect (1 2)

(list 'a 'b)
; expect (a b)

(list 'a b)
; expect (a 2)

(car '(a b c))
; expect a

(cdr '(a b c))
; expect (b c)

(define (memq item x)
  (cond ((null? x) false)
        ((equal? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
; expect False

(memq 'apple '(x (apple sauce) y apple pear))
; expect (apple pear)

(define (my-equal? x y)
  (cond ((pair? x) (and (pair? y)
                        (my-equal? (car x) (car y))
                        (my-equal? (cdr x) (cdr y))))
        ((null? x) (null? y))
        (else (equal? x y))))
(my-equal? '(1 2 (three)) '(1 2 (three)))
; expect True

(my-equal? '(1 2 (three)) '(1 2 three))
; expect False

(my-equal? '(1 2 three) '(1 2 (three)))
; expect False

;;; Peter Norvig tests (http://norvig.com/lispy2.html)

(define double (lambda (x) (* 2 x)))
(double 5)
; expect 10

(define compose (lambda (f g) (lambda (x) (f (g x)))))
((compose list double) 5)
; expect (10)

(define apply-twice (lambda (f) (compose f f)))
((apply-twice double) 5)
; expect 20

((apply-twice (apply-twice double)) 5)
; expect 80

(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
(fact 3)
; expect 6

(fact 50)
; expect 30414093201713378043612608166064768844377641568960512000000000000

(define (combine f)
  (lambda (x y)
    (if (null? x) nil
      (f (list (car x) (car y))
         ((combine f) (cdr x) (cdr y))))))
(define zip (combine cons))
(zip (list 1 2 3 4) (list 5 6 7 8))
; expect ((1 5) (2 6) (3 7) (4 8))

(define riff-shuffle (lambda (deck) (begin
    (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
    (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
    (define mid (lambda (seq) (/ (length seq) 2)))
    ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))
(riff-shuffle (list 1 2 3 4 5 6 7 8))
; expect (1 5 2 6 3 7 4 8)

((apply-twice riff-shuffle) (list 1 2 3 4 5 6 7 8))
; expect (1 3 5 7 2 4 6 8)

(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))
; expect (1 2 3 4 5 6 7 8)

;;; Additional tests

(apply square '(2))
; expect 4

(apply + '(1 2 3 4))
; expect 10

(apply (if false + append) '((1 2) (3 4)))
; expect (1 2 3 4)

(if 0 1 2)
; expect 1

(if '() 1 2)
; expect 1

(or false true)
; expect True

(or)
; expect False

(and)
; expect True

(or 1 2 3)
; expect 1

(and 1 2 3)
; expect 3

(and False (/ 1 0))
; expect False

(and True (/ 1 0))
; expect Error

(or 3 (/ 1 0))
; expect 3

(or False (/ 1 0))
; expect Error

(or (quote hello) (quote world))
; expect hello

(if nil 1 2)
; expect 1

(if 0 1 2)
; expect 1

(if (or false False #f) 1 2)
; expect 2

(define (loop) (loop))
(cond (false (loop))
      (12))
; expect 12

((lambda (x) (display x) (newline) x) 2)
; expect 2 ; 2

(define g (mu () x))
(define (high f x)
  (f))

(high g 2)
; expect 2

(define (print-and-square x)
  (print x)
  (square x))
(print-and-square 12)
; expect 12 ; 144

(/ 1 0)
; expect Error

(define addx (mu (x) (+ x y)))
(define add2xy (lambda (x y) (addx (+ x x))))
(add2xy 3 7)
; expect 13


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme Implementations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; len outputs the length of list s
(define (len s)
  (if (eq? s '())
    0
    (+ 1 (len (cdr s)))))
(len '(1 2 3 4))
; expect 4


;;;;;;;;;;;;;;;;;;;;
;;; Extra credit ;;;
;;;;;;;;;;;;;;;;;;;;

(exit)

; Tail call optimization tests

(define (sum n total)
  (if (zero? n) total
    (sum (- n 1) (+ n total))))
(sum 1001 0)
; expect 501501

(define (sum n total)
  (cond ((zero? n) total)
        (else (sum (- n 1) (+ n total)))))
(sum 1001 0)
; expect 501501

(define (sum n total)
  (begin 2 3
    (if (zero? n) total
      (and 2 3
        (or false
          (begin 2 3
            (let ((m n))
              (sum (- m 1) (+ m total)))))))))
(sum 1001 0)
; expect 501501
