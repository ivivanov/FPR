#lang racket
;Зад. 1. Да се дефинира процедура: а) (make-set A), която премахва повторенията от даден списък от числа A; б) (symmetric-difference A B), която намира симетричната разлика на множествата, представени чрез списъците A и B. 
;Зад. 2*. Като се използва процедурата от по-висок ред accumulate за работа с числови редици, да се дефинира процедура (derive-n f n eps), която намира n-та производна на едноаргументната реална функция f с точност eps. 
;Зад. 3. Като се използват функции от по-висок ред за работа със списъци, да се дефинира предикат (fixed? f ll), който по дадена реална едноаргументна функция f и списък от списъци от реални числа ll проверява дали поне в един от подсписъците на ll всички елементи са неподвижни точки за f (x е неподвижна точка за f, ако f(x) = x). Пример: > (fixed? (lambda (x) (+ (* x x) (- x 1))) ‘((0 1 -1) (1 -1) (0 -1))) #t 
;Зад. 4. Да се дефинира процедура (min-number-root2last-leafs tree), която намира най-малкото число, което може да се образува след последователно преминаване през върховете на дадено двоично дърво tree (с цифри по върховете) в посока от корена към листата, които са на последно ниво в tree.

(define setA '(1 2 3))
(define setB '(2 3 4 7))

;1
;a)
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

;(make-set '(1 1 2 2 3 1 3 1 4 5))

;b)
;;;;;;;;;;;;;;;;помощни процедури;;;;;;;;;;;;;;;;;;;;

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

(symmetric-difference setA setB)

;2
(define (accumulate1 combiner null_value term a next b)
  (if (> a b) null_value
      (combiner (term a)
                (accumulate1 combiner null_value term (next a) next b))
      )
  )

(define (repeat f n)
  (lambda (x)
    (if (= n 0) x
      (f ((repeat f (- n 1)) x)))))

(define (derive f eps)
  (lambda (x) (/(- (f (+ x eps))(f x)) eps))
  )

;(define (derive-n f n eps)
;  ((repeat (lambda (f) (derive f eps))n)f)
;  )

;((repeat (lambda(x)(+ x 1)) 10)0) 
;((derive-n (lambda(x)(expt x 4)) 3 0.0001)1)
;((derive-n (lambda(x)(* x x x)) 2 0.0001)2)


(define (derive-n f n eps)
  (accumulate1 (lambda(x) ((derive f eps)x))
               0
               (lambda(x) ((derive f eps)x))
               0
               (lambda(x)(+ 1 x))
               n))

(derive-n (lambda(x)(* x x x)) 2 0.0001)

;3

;;;;;;;;;;;;помощни функции;;;;;;;;;;;;;;;;;;;
(define (accumulate comb null_val l)
  (if (null? l) null_val
      (comb (car l) 
            (accumulate comb null_val (cdr l))
            )
      )
  )
(define (map f l)
  (if (null? l) '()
      (cons (f (car l))
            (map f (cdr l))
            )
      )
  )


(define (sum l)
  (if (null? l) 0
      (accumulate +
                  0
                  l)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (fixed? f ll)
   (cond ((null? ll) #f)
         ((= (sum (map f (car ll))) (sum (car ll))) #t)
         (else
          (fixed? f (cdr ll))
          )
         )
   )
                                             
    
 (fixed? (lambda (x) (+ (* x x) (- x 1))) '((0 1 -1) (1 -1) (0 -1)))
 (fixed? (lambda (x) x) '((0 1 -1) (1 -1) (0 -1)))
 (fixed? (lambda (x) (- 0 (* x x) )) '((0 1 -1)))
 (fixed? (lambda (x) (- 0 (* x x) )) '((0 -1)))
 
 ;4
 ; Да се дефинира процедура (level tree k), която по дадено двоично дърво tree намира списък от върховете на tree, които се намират на ниво k в tree (ще считаме, че коренът е на ниво 0).
;(define (level k tree)
;  (cond ((empty-tree? tree) '())
;        ((= k 0)(list (root tree)))
;        (else (append (level (- k 1) (left-tree tree))
;                      (level (- k 1) (right-tree tree))
;                      )
;              )
;        )
;  )