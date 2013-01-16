#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Домашно 3;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))
      )
  )
;;; Factorial
(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))
      )
  )

;зад 1
(define (sin x n)
  (accumulate + 
              0
              (lambda(i)(/
                         (*
                          (if (even? i) 1 -1)
                          (expt x (+ (* 2 i) 1))
                          )
                         (fact (+ (* 2 i) 1))
                         )
                )
              0
              (lambda(i)(+ i 1))
              n
              )
  )



;(sin (/ 3.1415 2) 10)

(define (sum-of-products n)
  (accumulate *
              1
              (lambda(x)(sin x n));(sin 0 1) = 0 , за това трябва а = 1
              1   ;а
              (lambda(x) (+ x 1))
              n
              )
  )
;(sum-of-products 5)

;Зад. 2*. Като се използва процедурата от по-висок ред accumulate, да се дефинира процедура: 
;а) (repeated f n), която намира n-кратна композиция на едноаргументната реална функция f; 
;б) (derive-n f n eps), която намира n-та производна на едноаргументната реална функция f с точност eps.
(define (repeated f n)
  (lambda(x)
    (if (= n 0) x
        (f ((repeated f (- n 1))x))
        )
    )
  ) 
        
;((repeated (lambda(x)(* x x)) 3)1)

(define (repeat f n)
  (lambda(x)
    (accumulate (lambda(x y)x)
                0
                f
                0
                (lambda(i)(+ i 1))
                n
                )
    )
  )
;((repeat (lambda(x)(* x x)) 3)1)
              

;Зад. 3. Да се дефинира процедура (make-set A), която премахва повторенията от даден списък от числа A.
(define setA (list 0 1 2 3))
(define setB (list 2 3 4 7))

(define (member? x l)
  (cond ((null? l) #f)
        ((eq? (car l) x) #t)
        (else (member? x (cdr l)))
        )
  )

(define (make-set A)
  (cond ((null? A) A)
        ((member? (car A) (cdr A)) (make-set (cdr A)))
        (else (cons (car A)(make-set (cdr A))))
        ))  
                  
;(make-set (list 1 2 3 3 0 0 3 2 2 2))

;б) (symmetric-difference A B), която намира симетричната разлика на множествата, 
;представени чрез списъците A и B.

;помощни процедури;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (union A B)
  (make-set (append A B)))

(define (intersection A B)
  (cond ((null? A) A)
        ((member? (car A) B)(cons (car A) (intersection (cdr A) B)))
        (else (intersection (cdr A) B))
        )
  )

(define (difference A B) 
  (cond ((null? A) A)
        ((member? (car A) B) (difference (cdr A) B))
        (else (cons (car A) (difference (cdr A) B)))
        )
  )
;A (+) B = (A\B)U(B\A) еквивалентно на A (+) B = (А U B)\(A П B) 
;Ще използвам първото уравнение
(define (symmetric-difference A B)
  (union (difference A B) (difference B A)))

;(symmetric-difference setA setB)