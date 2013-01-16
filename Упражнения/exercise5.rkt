#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Упражнение 5;;;;;;;;;;;;;;;;;;;;;
;;;Accumulate
;Иреративен вариант
(define (accumulate combiner null_value term a next b)
  (define (accumulate-iter i res)
    (if (> i b)res
        (accumulate-iter (next i)(combiner (term i) res))
        )
    )
  (accumulate-iter a null_value)
  )

;Рекурсивен вариант
(define (accumulate2 combiner null_value term a next b)
  (if (> a b) null_value
      (combiner (term a)
                (accumulate2 combiner null_value term (next a) next b))
      )
  )
;;;Accumulate + filter 

;Иреративен вариант
(define (filter-accum pred combiner null_value term a next b)
  (cond ((> a b) null_value)
        ((pred (term a))(combiner (term a)(filter-accum pred combiner null_value term (next a) next b)))
        (else (filter-accum pred combiner null_value term (next a) next b))
        )
  )
;Рекурсивен вариант
(define (filter-accum2 pred combiner null_value term a next b)
  (define (filter-iter i res)
    (cond ((> i b) res)
          ((pred (term i))(filter-iter (next i) (combiner res (term i))))
          (else (filter-iter (next i) res))
          )
    )
  (filter-iter a null_value)
  )
  

;<term> е процедура за намиране на общия член на сумата
;<next> е процедура за получаване на стъпката
;<combainer> е оператор : +, -, *, /, ...
;<null_value> е начална стойност (нулев елемент за оператора. пр.: за * null_value = 1)
;<pred> - от предикат, натрупват се само тези елементи които удоволетворяват условието.

;Зад 1

;а)
(define (fact n)
  (accumulate * 1 (lambda(x)x) 1 (lambda(x) (+ x 1)) n)
  )

(fact 4)

;б)
(define (pow x n)
  (accumulate * 1 (lambda(i)x) 1 (lambda(i) (+ i 1))n)
  )
(pow 2 3)

;в)
;Лесен начин
(define (V n k)
  (/ 
   (accumulate * 1 (lambda(x) x) 1 (lambda(x) (+ x 1)) n)
   (accumulate * 1 (lambda(x) x) 1 (lambda(x) (+ x 1)) (- n k))
   )
  )
;Оптимизиран
(define (V-opti n k)
  (accumulate * 1 (lambda(x) x) (+ (- n k) 1) (lambda(x) (+ x 1)) n)
  )

(V 5 3)
(V-opti 5 3)

;г)
;чрез (V-opti n k) и (fact n)
(define (C n k)
  (/ (V-opti n k)(fact k))
  )

;чрез accumulate без оптимизация
(define (C-1 n k)
  (/   
   (accumulate * 1 (lambda(x) x) 1 (lambda(x) (+ x 1)) n)
   (* 
    (accumulate * 1 (lambda(x) x) 1 (lambda(x) (+ x 1)) k)
    (accumulate * 1 (lambda(x) x) 1 (lambda(x) (+ x 1)) (- n k))
    )
   )
  )
;Оптимизирант вариант
(define (C-opti n k)
  (accumulate * 1 (lambda(i)(/ (+ (- n i) 1) i)) 1 (lambda(i)(+ i 1)) k)
  )

(C 5 3)
(C-1 5 3)
(C-opti 5 3)

;д)
(define (exp x n)
  (accumulate +
              0
              (lambda(i)(/ (pow x i)(fact i))) 
              0
              (lambda(i)(+ i 1)) 
              n
              )
  )

(exp 3 10)

;е)
(define (sin x n)
  (accumulate +
              0
              (lambda(i)(/
                         (*(pow -1 i)(pow x (+ (* 2 i) 1)))
                         (fact (+ (* 2 i) 1))
                         ))
              0
              (lambda(i) (+ i 1))
              n)
  )
(sin 1 5)
;ж)
(define (cos x n)
  (accumulate +
              0
              (lambda(i)(/
                         (*(pow -1 i)(pow x (* 2 i)))
                         (fact (* 2 i))
                         ))
              0
              (lambda(i)(+ i 1))
              n)
  )
(cos 0 10)

;з)
(define (sum-of-products n)
  (accumulate +
              0
              (lambda(k)(*
                         (pow -1 (- k 1))
                         (accumulate *
                                     1
                                     (lambda(i)(/
                                                (- (* 2 i) 1)
                                                (* 2 i)))
                                     1
                                     (lambda(i) (+ i 1))
                                     k))
                )
              1
              (lambda(i)(+ i 1))
              n)
  )
       
;и)
;В задачата се иска да се намери произведението от 1 до к на (sin x n)
(define (product-of-sin n)
  (accumulate *
              1
              (lambda(x)(sin x n))
              1
              (lambda(i)(+ i 1))
              n
              )
  )
;зад 2
(define (integral f a b n)
  (let ((h (/ (- b a) n)))
    (* (/ h 3)
       (+ (f a)(f b)(* 
                     4
                     (accumulate +
                                 0
                                 (lambda(i)(f (+ a(* h (+ (* 2 i) 1)))))
                                 0
                                 (lambda(i)(+ i 1))
                                 (/ (- n 2) 2)
                                 )
                     )
          (* 2 (accumulate +
                           0
                           (lambda(i) (f (+ a (* 2 i h))))
                           1
                           (lambda(i)(+ i 1))
                           (/ (- n 2) 2)))
          )
       )
    )
  )
(integral (lambda(x) (/ (* x x x)(+ x 1))) 0 1 2)