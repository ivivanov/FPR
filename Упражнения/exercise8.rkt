#lang racket
;;;;;;;;;;;;;;;;;;;; Упражнение 8 ;;;;;;;;;;;;;;;;;;;;

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
(define (filter pred? l)
  (cond ((null? l) '())
        ((pred? (car l))(cons (car l)(filter pred? (cdr l))))
        (else (filter pred? (cdr l)))
        )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;зад 1
;а)
(define (len l)
  (if (null? l) 0
      (+ 1 (len (cdr l)))
      )
  )
(len '(1 2 3))
;б) сумата от квадратите на нечетните числа в списъка от естествени числа l. 

(define (func1 l)
  (accumulate +
              0
              (map 
               (lambda(x)(* x x)) 
               (filter odd? l)
               )
              )
  )

(func1 (list 1 2 3 4 5))

;зад 2 

(define (quick-sort l)
  (if (<= (len l) 1) l
      (append 
       (quick-sort (filter (lambda(x)(< x (car l))) l)) 
       (list (car l))
       (quick-sort (filter (lambda(x)(> x (car l))) l))
       )
      )
  )
          
(quick-sort '(3 2 1 4 8 9 0))

;зад 3
(define (scons x ll)
  (map (lambda(i) (cons x i)) ll))
(scons 1 '((2 3)(4 5)))

;зад 4
;Да се дефинира процедура (permutations l), която намира списък от списъци, съдържащи пермутациите на елементите на списъка l.
(define (exclude x l)
  (filter (lambda(i)(not(= x i))) l))

(exclude 3 '(1 2 3 4))

(define (permutations l)
  (if(<= (len l) 1) (list l)
     (accumulate append 
                 '()
                 (map (lambda(x)(scons x(exclude x l))) l) 
                 )
     )
  )
(permutations '(1 2 3))

;зад 5
(define (prime? n)
  (define (iter i counter)
    (cond ((> i n) (if (= counter 2) #t #f))
          ((= (remainder n i) 0)(iter (+ i 1)(+ 1 counter)))
          (else (iter (+ 1 i) counter))
          )
    )
  (iter 1 0)
  ) 

(prime? 12)

(define (number2primes n)
  (define (iter i)
    (cond ((> i n) '())
          ((and (prime? i) (prime? (- n i))) (list i (- n i)))
          (else (iter (+ i 1)))
          )
    )
  (iter 1)
  )
(number2primes 13)

(define (list2primes l)
  (map number2primes (filter even? l))
  )
(list2primes '(7 8 6 9 12 16))

;зад 6
;без accumulate
(define (composition1 l)
  (if (null? l)(lambda(x) x)
      (lambda(x)((car l)((composition1 (cdr l))x))
        )
      )
  )
;с accumulate
(define (composition l)
  (accumulate (lambda(f g)(lambda(x)(f (g x))))
              (lambda(x)x)
              l))

;((composition1 '((lambda(x)(+ x 1)) (lambda(x)(+ x 2)) ))3)

;зад 7
(define (converter x)
  (cond ((eq? x 0) 0000)
        ((eq? x 1) 0001)
        ((eq? x 2) 0010)
        ((eq? x 3) 0011)
        ((eq? x 4) 0100)
        ((eq? x 5) 0101)
        ((eq? x 6) 0110)
        ((eq? x 7) 0111)
        ((eq? x 8) 1000)
        ((eq? x 9) 1001)
        ((eq? x 'A) 1010)
        ((eq? x 'B) 1011)
        ((eq? x 'C) 1100)
        ((eq? x 'D) 1101)
        ((eq? x 'E) 1110)
        ((eq? x 'F) 1111)
        (else 0)
        )
  )
(converter 'F)
(define (hex2bin l)
  (map converter l))
(hex2bin '(A 2 E))