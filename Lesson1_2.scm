;;;; Linear recursive factorial
(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))
    )
)

;;;; Linear iterative factorial
(define (factorial n)
    (fact-iter 1 1 n)
)

(define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product) (+ counter 1) max-count)
    )
)

;;;; Exercise 1.9
;;;; Each of the following two procedures defines 
;;;; a method for adding two positive integers in terms 
;;;; of the procedures inc, which increments its argument by 1, and dec, which decrements its argument by 1.
(define (inc x)
    (+ 1 x)
)

(define (dec x)
    (- x 1)
)

(define (+ a b)
    (if (= a 0)
        b
        (inc (+ (dec a) b))
    )
)

(define (+ a b)
    (if (= a 0)
        b
        (+ (dec a) (inc b))
    )
)
;;;; Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5). Are these processes iterative or recursive?
;;;; a. (+ 4 5)
;;;;    (inc (+ (dec 4) 5))
;;;;    (inc (+ 3 5))
;;;;    (inc (inc (+ (dec 3) 5)))
;;;;    (inc (inc (+ 2 5)))
;;;;    (inc (inc (inc (+ (dec 2) 5))))
;;;;    (inc (inc (inc (+ 1 5))))
;;;;    (inc (inc (inc (inc (+ (dec 1) 5)))))
;;;;    (inc (inc (inc (inc (+ 0 5)))))
;;;;    (inc (inc (inc (inc 5))))
;;;;    (inc (inc (inc 6)))
;;;;    (inc (inc 7))
;;;;    (inc 8)
;;;;    9
;;;; b. (+ 4 5)
;;;;    (+ (dec 4) (inc 5))
;;;;    (+ 3 6)
;;;;    (+ (dec 3) (inc 6))
;;;;    (+ 2 7)
;;;;    (+ (dec 2) (inc 7))
;;;;    (+ 1 8)
;;;;    (+ (dec 1) (inc 8))
;;;;    (+ 0 9)
;;;;    9
;;;; a is a recursive process, where b is an iterative process.

;;;; Exercise 1.10
;;;; The following procedure computes a mathematical function called Ackermann's function.
(define (A x y)
    (cond
        ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else
            (A (- x 1) (A x (- y 1)))
        )
    )
)
;;;; What are the values of the following expressions?
(A 1 10)
;;;; (A 1 10)
;;;; (A 0 (A 1 9))
;;;; (A 0 (A 0 (A 1 8))) 
;;;; (A 0 (A 0 (A 0 (A 1 7))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;;;; (A 0 (A 0 (A 0 (A 0 64))))
;;;; (A 0 (A 0 (A 0 128)))
;;;; (A 0 (A 0 256))
;;;; (A 0 512)
;;;; 1024 = 2^10
(A 2 4)
;;;; (A 2 4)
;;;; (A 1 (A 2 3))
;;;; (A 1 (A 1 (A 2 2)))
;;;; (A 1 (A 1 (A 1 (A 2 1))))
;;;; (A 1 (A 1 (A 1 2)))
;;;; (A 1 (A 1 (A 0 (A 1 1))))
;;;; (A 1 (A 1 (A 0 2)))
;;;; (A 1 (A 1 4))
;;;; (A 1 (A 0 (A 1 3)))
;;;; (A 1 (A 0 (A 0 (A 1 2))))
;;;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;;;; (A 1 (A 0 (A 0 (A 0 2))))
;;;; (A 1 (A 0 (A 0 4)))
;;;; (A 1 (A 0 8))
;;;; (A 1 16)
;;;; (A 0 (A 1 15))
;;;; (A 0 (A 0 (A 1 14)))
;;;; (A 0 (A 0 (A 0 (A 1 13))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 1 12)))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6))))))))))) 
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5)))))))))))) 
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
;;;; (A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
;;;; (A 0 (A 0 (A 0 (A 0 4096))))
;;;; (A 0 (A 0 (A 0 8192)))
;;;; (A 0 (A 0 16384))
;;;; (A 0 32768)
;;;; 65536 = 2^(2^4)
(A 3 3) 
;;;; (A 2 (A 3 2))
;;;; (A 2 (A 2 (A 3 1)))
;;;; (A 2 (A 2 2))
;;;; (A 2 (A 1 (A 2 1)))
;;;; (A 2 (A 1 2))
;;;; (A 2 4)
;;;; ...
;;;; 65536 = how does this work??? freaking magic. Check the wikipedia article, read up on Knuth's Up-Arrow Notation
;;;; Consider the following procedures, where A is the procedure defined above:
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
;;;; Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values of n. For example, (k n) computes 5(n^2).
;;;; f(n) = 2n
;;;; g(n) = 2^n, 0 for n = 0
;;;; h(n) = 2^(2^n), 0 for n - 0?

;;;; Exercise 1.12
;;;; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>=3. 
;;;; Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.
(define (f n)
    (if (< n 3)
        n
        (+ 
            (f (- n 1)) 
            (* 2 (f (- n 2))) 
            (* 3 (f (= n 3)))
        )
    )
)

;;;; Stole this one from the wiki, will step through to see how it works
(define (f n)
    (define (f-iter a b c n1)
        ;; a = f(n - 1), b = f(n - 2), c = f(n - 3)
        ;; return a + 2b + 3c
        (if (< n1 3)
            a
            (f-iter
                (+ a (* 2 b) (* 3 c))
                a
                b
                (- n1 1)
            )
        )
    )
    (if (< n 3)
        n
        (f-iter 2 1 0 n)
    )
)
;;;; f(4)
;;;; (f-iter 2 1 0 4)
;;;; (f-iter (+ 2 2 0) 2 1 3)
;;;; (f-iter 4 2 1 3)
;;;; (f-ter (+ 4 4 3) 4 2 2)
;;;; (f-iter 11 7 2 2)
;;;; 11
;;;; f(7)
;;;; (f-iter 2 1 0 7)
;;;; (f-iter (+ 2 2 0) 2 1 6)
;;;; (f-iter 4 2 1 6)
;;;; (f-iter (+ 4 4 3) 4 2 5)
;;;; (f-iter 11 4 2 5)
;;;; (f-iter (+ 11 8 6) 11 4 4)
;;;; (f-iter 25 11 4 4)
;;;; (f-iter (+ 25 22 12) 25 11 3)
;;;; (f-iter 59 25 11 3)
;;;; (f-iter (+ 59 50 33) 59 25 2)
;;;; (f-iter 142 59 25 2)
;;;; 142
;;;; So 2 is f(3 - 1), 1 is f(3 - 2), and 0 is f(3 - 3)
;;;; Looks like we build up the sum of each step as we go, though I'm still not fully sure how this was derived.
;;;; Okay, so I went and sort of started an iterative version in Csharp, and I think I get it better.
;;;; First iteration: a b c are equal to f(3 - 1), f(3 - 2), and f(3 - 3)
;;;; a becomes f(3), b becomes f(3 - 1), c becomes f(3 - 2), n is decremented and serves as a counter (we'll stop when it's less than 3, as we started calculating from n = 3)
;;;; a becomes f(4), b becomes f(4 - 1), c becomes f(4 - 2), n is decremented
;;;; ...
;;;; a becomes f(7), b becomes f(7 - 1), c becomes f(7 - 2), n is decremented, to 2, so we stop
;;;; a is returned, so the procedure returns f(7), as expected.

;;;; Exercise 1.12
;;;; Write a recursive process to generate elements of Pascal's triangle
;;;; Apparently, this mercs scheme real hard
(define (pascals-tri row col)
    (cond
        ((or (= row 1) (= col row) (= col 1)) 1)
        (else
            (+ (pascals-tri (- row 1) (- col 1))
                (pascals-tri (- row 1) col)
            )
        )
    )
) 
;;;; Okay, so let's trace this out
;;;; (pascals-tri 3 2)
;;;; (+ (pascals-tri 2 1) (pascals-tri 2 2))
;;;; (+ (1) 1)
;;;; 2
