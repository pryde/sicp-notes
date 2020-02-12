(define (square x)
    (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (abs x)
    (cond ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) -x)))

;;;; Exercise 1.1
;;;; Evaluate each expression.
10                                 ;;=> 10
(+ 5 3 4)                          ;=> 12
(- 9 1)                            ;=> 8
(/ 6 2)                            ;=> 3
(+ (* 2 4) (- 4 6))                ;=> 6
(define a 3)
(define b (+ a 1))                 ;=> b = 4
(+ a b (* a b))                    ;=> 19
(= a b)                            ;=> false
(if (and (> b a) (< b (* a b)))
   a
   b)                              ;=> a = 3
(cond ((= a 4) 6)
   ((= b 4) (+ 6 7 a))
   (else -1))                      ;=> 13
(+ 2 (if (> b a) b a))             ;=> 6
(* (cond ((> a b) a)
       (< a b) b)
       (else -1)
   (+ a 1))                        ;=> 16

;;;; Exercise 1.2
;;;; Translate the expression into prefix form: (5 + (2 - (3 - (6 + 1/5)))) / 3 * (6 - 2) * (2 - 7)
(/ 5 (- 2 (- 3 (+ 6 (/ 1 5))))
   (* 3 (- 6 2) (- 2 7)))

;;;; Exercise 1.3
;;;; Define a procedure that takes three arguments and
;;;; returns the sum of the squares of the larger numbers
;;;; TODO: make it work in the case where y = z and y < x
(define (sum-of-largest-squares x y z)
    (cond ((and (> x z) (> y z)) (sum-of-squares x y))
          ((and (> x y) (> z y)) (sum-of-squares x z))
          (else (sum-of-squares y z))))

;;;; Exercise 1.4
;;;; Describe the behavior of the following
(define (a-plus-abs-b a b)
        ((if (> b 0) + -) a b))
;;;; If b is negative, then perform a - b, else perform a + b
;;;; Always returns positive result

;;;; Exercise 1.5
;;;; Evaluate the following with a) applicative order evaluation, then b) normal order evaluation
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
;;;; a) Using applicative order evaluation:
;;;;        First, evaluate sub-expressions:
;;;;            0 resolves to 0
;;;;            (p) resolves to (p). This results in an infinite loop.
;;;; b) Using normal order evaluation:
;;;;        First, expand:
;;;;            (test 0 (p)) => (if (= 0 0) 0 (p))
;;;;                (= 0 0) => true, so (p) is never evaluated. The procedure returns 0

;;;; Newton's Method of Successive Approximations
(define (sqrt-iter guess x)
    (if good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
            x))

;;;; Improve the approximation y by averaging y and x/y
(define (improve guess x)
    (average guess (/ x guess)))

;;;; Average two numbers
(define (average x y)
    (/ (+ x y) 2))

;;;; Tolerance test for approximations
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

;;;; Always guess the sqrt is 1, like a fool
(define (sqrt x)
    (sqrt-iter 1.0 x))

;;;; TODO: WHY DOES THE ABOVE NOT FREAKING WORK? COME BACK TO THESE EXERCISES

;;;; Exercise 1.6
;;;; If the sqrt-iter procedure above was re-writting using the below new-if, what happens?
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;;;; Since the new-if does not follow the same evaluation rules as the special form 'if'
;;;; the else clause will always be evaluated, resulting in infinite recursion

;;;; Exercise 1.7
;;;; Implement a better 'good-enough?' test, which stops the test when the change in 'guess'
;;;; is a small fraction of the total
;;;; From Scheme wiki:
(define (better-good-enough? guess x)
    (= (improve guess x) guess))

;;;; Testing?
(define (sqrt-iter guess x)
  (if (better-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
;;;; EXPLANATION: The above will converge to the appropriate value within the limits of the 
;;;; system's precision

;;;; Exercise 1.8
;;;; Implement a cube root solution analogous to the above square root method
(define (cube-root-iter guess x)
    (if (better-good-enough? guess x) 
       guess 
       (cube-root-iter (c-improve guess x) 
                        x)))

(define (c-improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess))
    3))