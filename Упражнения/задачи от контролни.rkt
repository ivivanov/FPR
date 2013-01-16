#lang racket
;;;;;Зимен семестър 2009/2010
;;;;;3 курс, Информатика
;;;;;първо контролно



;;;;;;;;;;;;;;;;;;;;;;;;;;Комплект 1 – Вариант 1;;;;;;;;;;;;;;;;;;

;1 зад
(define (count-digits n)
  (if (< n 10) 1
      (+ 1(count-digits (quotient n 10)))
      )
  )
                  
(define (make-number n)
  (define (iter number result exptNum)
    (cond
      ((= (count-digits n) 1) n)
      ((= (count-digits number) 1) result)
      ((even? (count-digits number)) (iter (quotient number 10) (+ result (* exptNum (remainder number 10))) (* exptNum 10)))
      (else (iter (quotient number 10) result exptNum))
      )
    )
  (iter n 0 1)
  )

;(make-number 1110)
;(make-number 1)

;2 зад
;a)
(define (accum-filter pred comb null-val term a next b)
  (define (iter i result)
    (cond 
      ((> i b)result)
      ((pred (term i))(iter (next i)(comb result (term i))))
      (else (iter (next i) result))
      )
    )
  (iter a null-val)
  )
        
;б)
(define (isPrime? n )
  (if(= (+ 1 n)(accum-filter (lambda(x)(if (= (remainder n x) 0) true false))
                             +
                             0
                             (lambda(x) x)
                             1
                             (lambda(x) (+ x 1))
                             n)
        ) true false)) 
;(isPrime? 7)

;в)
(define (sum-number a b)
  (accum-filter (lambda(x)true)
                +
                0
                (lambda(x)(if(isPrime? x)(make-number x)0))
                a
                (lambda(x)(+ 1 x))
                b
                )
  )
(sum-number 10 20)
;3 зад
;а)
(define (derive f)
  (lambda(x) (/ (- (f (+ x 0.0001)) (f x)) 0.0001)))

;((derive (lambda(x)(* x x x)))2)

;б)
(define (derive-n f n)
  (if (= n 0) f
      (derive-n (derive f) (- n 1))
      )
  )

;((derive-n (lambda(x)(* x x x)) 2)2)

;в)
(define (fact n)
  (accum-filter (lambda(x) true) 
                *
                1
                (lambda(x) x)
                1
                (lambda(x) (+ x 1))
                n)
  )
;(fact 4)

(define (T n a f)
  (lambda(x)
    (accum-filter (lambda(x) true) 
                  +
                  0
                  (lambda(i) (*
                              ((derive-n f i)a) 
                              (/ 
                               (expt (- x a) i) 
                               (fact i))
                              )
                    )
                  0
                  (lambda(i)(+ i 1))
                  n
                  )
    )
  )
; ((T 5 1 (lambda(x)(* x x x)))2)