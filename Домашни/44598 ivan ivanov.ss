;;;;;;;;;;;;;;;;;;;;;;;;;;;;ДОМАШНО УПРАЖНЕНИЕ 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Да се дефинира процедура, която проверява дали дадена точка (x, y) принадлежи на 
;сечението на кръг с център (a, -a) и радиус a, a>0,
;с квадрат с върхове (0, 0), (a, 0), (a, -a) и (0, -a).
;Решение:
; Условията които трябва да изпълняват координатите х и у на точката (х,у)
; 1) 0 < x < a    
; 2) -a < y < 0   
; 3) d(x,y)(a,-a) < a , където d е разстоянието от точка (х,у) до центъра на окръжността
; за 3 използвам формулата d=sqrt( (x-a)^2 + (y + a)^2 )

#lang racket
(define (pointInAreaX? x y a)
  (if (and
       (and (> x 0)(< x a))
       (and (> y (- 0 a))(< y 0))
       (<(sqrt (+ (*(- x a)(- x a))(*(- y (- 0 a))(- y (- 0 a))))) a)
      )true false
   )
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;ДОМАШНО УПРАЖНЕНИЕ 2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Задача за домашно: 
;а) Да се дефинира предикат (perfect? n), който проверява дали дадено естествено число n е съвършено. Забележка: Числото n е съвършено,
;ако е равно на сумата от своите делители (включително 1, без n). Например 6 = 1 + 2 + 3 и 28 = 1 + 2 + 4 + 7 + 14 са съвършени. 
;б) Да се дефинира процедура (n-th-perfect n), която намира n-тото съвършено число.

;a)
(define (perfect? n)
  (define (perfect-iter sumOfDiv i)
    (cond 
      ((= i n) (if (= sumOfDiv n)true false))
       ((= (remainder n i) 0) (perfect-iter (+ sumOfDiv  i) (+ i 1)))
      (else (perfect-iter sumOfDiv (+ i 1)))
      )
    )
  (perfect-iter 0 1)
  )

;b)
(define (n-th-perfect n)
  (define (iter i counter)
    (cond 
      ((= counter n) (- i 1))
      ((perfect? i)(iter (+ 1 i) (+ 1 counter)))
      (else (iter (+ 1 i) counter))))
  (iter 1 0)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;ДОМАШНО УПРАЖНЕНИЕ 3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Зад. 7. (за домашно) Да се дефинира процедура (replace n a b), която замества в естественото число n всички срещания на цифрата a с цифрата b. 
;Зад. 8. (за домашно) Да се дефинира процедура (hermite-polynomial r x), която генерира итеративен процес 
;за пресмятането на стойността на полинома на Ермит в точката x (Hr(x)), зададен чрез формулата: H0(x) = 1, H1(x) = 2x, Hr+1(x) = 2xHr(x) - 2rHr-1(x). 

;zad7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;spomagatelni funkcii;;;;;;;;;;;;;;;;;;;;;;
(define (reverse n)
  (define (reverse-iter newNum num i)
    (if (= i 0) newNum (reverse-iter (+ newNum (* (remainder num 10) (pow 10 (- i 1)))) (quotient num 10) (- i 1))))
  (reverse-iter 0 n (count-digits n))
  )

(define (count-digits n)
  (if (< (quotient n 10) 1) 1 (+ 1 (count-digits (quotient n 10)))))

(define (pow x n)
  (cond ((= n 0) 1)
        ((= n 1) x)
        (else (* x (pow x (- n 1))))))
      
(define (zeroInTheEndCounter n)
  (cond 
    ((= (count-digits n) 1) 0)
    ((= (remainder n 10)0) (+ 1 (zeroInTheEndCounter (quotient n 10))))
    ((not(= (remainder n 10)0)) 0)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Reshenie;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (replace n a b)
  (define (replace-iter i num newNum zeros)
    (cond 
      ((= i 0) (if (= a 0) (reverse newNum)
                    (*(reverse newNum)(pow 10 zeros))))
      ((= (remainder num 10) a) 
       (replace-iter (- i 1) 
                     (quotient num 10) 
                     (+ newNum (* b (pow 10 (- i 1))))
                     zeros))
      (else 
       (if 
        (= (remainder num 10) 0)
        (replace-iter (- i 1) (quotient num 10) newNum zeros)
        (replace-iter (- i 1) 
                      (quotient num 10) 
                      (+ newNum (*(remainder num 10)(pow 10 (- i 1))))
                      zeros)
        )
       )
      )
    )
  (replace-iter (count-digits n) n 0 (zeroInTheEndCounter n))
  )

;zad8  Hr+1(x) = 2xHr(x) - 2rHr-1(x) Hn(x) = 2xHn-1(x) - 2(n-1)Hn-2(x)
(define (Hermite n x)
  (define (Hermite-iter hr_2 hr_1 i)
    (cond 
      ((= n 0) hr_2)
      ((= n 1) hr_1)
      ((= i n)(- 
               (* 2 x hr_1)
               (* 2 (- n 1) hr_2)))
      ((< i n)(Hermite-iter hr_1 
                            (- (* 2 x hr_1)
                               (* 2 (- i 1) hr_2))
                            (+ 1 i)))
      )
    )
(Hermite-iter 1 (* 2 x) 2)
)
;rekursiven
(define (Hr n x)
  (cond ((= n 0)1)
        ((= n 1)(* 2 x))
        (else (- (* 2 x(Hr (- n 1) x))
                 (* 2 (- n 1)(Hr (- n 2) x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;uprajnenie 4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Зад. 9. (за домашно) Да се дефинират процедури от по-висок ред (derive-x-n f n eps) и (derive-y-n f n eps), които намират съответно n-та частна производна
;по x и n-та частна производна по y на двуаргументната реална функция f с точност eps: а) без използване на процедурата (derive-n f n eps) от зад. 6
; б) (за бонус) с използване на процедурата (derive-n f n eps) от зад. 6. 
;Зад. 10. (за домашно - за бонус) Да се дефинира процедура от по-висок ред (derive-n f n eps),
;която намира n-та производна на едноаргументната реална функция f с точност eps чрез използване на процедурата repeated от зад. 7.  

;zad9
(define (derive f eps)
  (lambda (x) (/(- (f (+ x eps))(f x)) eps))
  )

(define (derive-n f n eps)
  ((repeated (lambda (f) (derive f eps))n)f)
  )

(define (repeated f n)
  (cond ((= n 0)(lambda (x) x))
        ((= n 1) f)
        (else (compose f (repeated f (- n 1))))
        )
  )
(define (repeat f n)
  (lambda (x y)
    (if (= n 0) f
      (f ((repeat f (- n 1)) x y)))))
(define (repeate f n)
  (cond ((= n 0)(lambda (x y) x))
        ((= n 1) f)
        (else (comp f (repeate f (- n 1))))
        )
  )
(define (comp f g)
  (lambda (x y)
    (f (g x y))
    )
  )


(define (compose f g)
  (lambda (x)
    (f (g x))
    )
  )

(define (derive-x f eps)
  (lambda (x y)(/ (- (f (+ x eps) y)(f x y))
                  eps)))

(define (derive-y f eps)
  (lambda (x y)(/ (- (f x (+ y eps))(f x y))
                  eps)))
;Искам n пъти да приложа derive-x върху f
;Така мисля че трябва да стане :
(define (derive-x-n1 f n eps)
  (lambda (f) (repeated 
                 (lambda (f) (derive-x f eps))
                 n)
    f))
;(define (derive-x-n f n eps)
;  (lambda (x y)
;((derive-x (lambda(x y) (+ x y)) 0.0001) 1 1)
