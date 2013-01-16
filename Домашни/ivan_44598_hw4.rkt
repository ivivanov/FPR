#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;uprajnenie 4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Зад. 9. (за домашно) Да се дефинират процедури от по-висок ред (derive-x-n f n eps) и (derive-y-n f n eps), които намират съответно n-та частна производна
;по x и n-та частна производна по y на двуаргументната реална функция f с точност eps: а) без използване на процедурата (derive-n f n eps) от зад. 6
; б) (за бонус) с използване на процедурата (derive-n f n eps) от зад. 6. 
;Зад. 10. (за домашно - за бонус) Да се дефинира процедура от по-висок ред (derive-n f n eps),
;която намира n-та производна на едноаргументната реална функция f с точност eps чрез използване на процедурата repeated от зад. 7.  

;zad9
;a)
(define (derive-x f eps)
  (lambda (x y)
    (/ (- (f(+ x eps)y) (f x y))eps)
    ))

(define (derive-y f eps)
  (lambda (x y)
    (/ (- (f x (+ y eps)) (f x y))eps)
    ))

(define (derive-x-n f n eps)
  (lambda (x y)
    (if(- n 1)((derive-x f eps)x y)
       ((derive-x (derive-x-n f (- n 1)eps)eps)x y)
       )))

(define (derive-y-n f n eps)
  (lambda (x y)
    (if(- n 1)((derive-y f eps)x y)
       ((derive-x (derive-y-n f (- n 1)eps)eps)x y)
       )))
;zad10
(define (repeat f n)
  (lambda (x y)
    (if (= n 0) f
      (f ((repeat f (- n 1)) x y)))))

(define (derive f eps)
  (lambda (x) (/(- (f (+ x eps))(f x)) eps))
  )

(define (derive-n f n eps)
  ((repeat (lambda (f) (derive f eps))n)f)
  )