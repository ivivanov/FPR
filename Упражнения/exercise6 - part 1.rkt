#lang racket
;;;;;;;;;;;;;;;;;;;;;;;Упражнение 6;;;;;;;;;;;;;;;;;;;;
;;;Accumulate - подробно обяснение в упр. 5
;Иреративен вариант
(define (accumulate combiner null_value term a next b)
  (define (accumulate-iter i res)
    (if (> i b)res
        (accumulate-iter (next i)(combiner (term i) res))
        )
    )
  (accumulate-iter a null_value)
  )
;;;Accumulate + filter 
(define (filter-accum pred combiner null_value term a next b)
  (cond ((> a b) null_value)
        ((pred (term a))(combiner (term a)(filter-accum pred combiner null_value term (next a) next b)))
        (else (filter-accum pred combiner null_value term (next a) next b))
        )
  )

;;; C n k (n над к)
(define (C n k)
  (accumulate * 1 (lambda(i)(/ (+ (- n i) 1) i)) 1 (lambda(i)(+ i 1)) k)
  )
;;;n!
(define (fact n)
  (accumulate * 1 (lambda(x)x) 1 (lambda(x) (+ x 1)) n)
  )

;;;Производна
(define (repeat f n)
  (lambda (x)
    (if (= n 0) x
      (f ((repeat f (- n 1)) x)))))

(define (derive f eps)
  (lambda (x) (/(- (f (+ x eps))(f x)) eps))
  )
;;;;;;;
(define (derive-n f n eps)
  ((repeat (lambda (f) (derive f eps))n)f)
  )
;;;;;;;
;зад 1
(define (forward-difference f x k h)
  (accumulate +
              0
              (lambda(j)(* 
                         (if (even? (- k j)) 1 -1)
                         (C k j)
                         (f (+ x (* j h)))
                         )
                )
              0
              (lambda(j)(+ j 1))
              k
              )
  )

;зад 2
(define (jacobi-poly n alpha beta)
  (lambda(x)(*
             (/ (if (even? n) 1 -1) (* (expt 2 n)(fact n)))
             (expt (- 1 x) (- 0 alpha))
             (expt (+ 1 x) (- 0 beta))
             ((derive-n (lambda(y)(*
                                   (expt (- 1 x)(+ alpha n))
                                   (expt (+ 1 x)(+ beta n)))
                          )
                        n
                        0.001)x)
             )
    )
  )

;зад 3
(define (taylor f a n)
  (lambda(x)(
             (accumulate +
                         0
                         (lambda(i)(*
                                    ((derive-n f i 0.0001)a)
                                    (/ (expt (- x a) i)(fact i))
                                    )
                           )
                         0
                         (lambda(i)(+ i 1))
                         n)
             )
    )
  )
 ((taylor (lambda(x)(* x x x)) 1 5 )2) 
                
;зад 4 Зад. 4. Като се използва процедурата accumulate, да се дефинира предикат: 
;      а) (prime? n), който проверавя дали естественото число n е просто; 
;      б) (perfect? n), който проверавя дали естественото число n е съвършено
;а)
(define (prime? n)
  (if (= (+ 1 n) (filter-accum (lambda(x)(if (= (remainder n x) 0) true false))
                               +
                               0
                               (lambda(x) x) 
                               1
                               (lambda(x)(+ 1 x))
                               n
                               ))
      true
      false
      )
  )
(prime? 8);не
(prime? 17);да

;б)
(define (perfect? n)
  (if (= n (filter-accum (lambda(x) (if (= (remainder n x) 0) true false))
                         +
                         0
                         (lambda(x)x)
                         1
                         (lambda(x) (+ x 1))
                         (- n 1)
                         )
         )
      true
      false
      )
  )
(perfect? 6);да
(perfect? 28);да
(perfect? 66);не