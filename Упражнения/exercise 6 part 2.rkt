#lang racket

;;;;;;;;;;;;;;;;;;;;cons car cdr cdar Упражнение 6 - част 2 ;;;;;;;;;;;;;;;;;;;;;;;

;(newline)
;(print "cons 5 6")
;(cons 5 6)
;(define x (cons 5 6))
;(print "car x")
;(car x)
;(print "cdr x")
;(cdr x)
;
;(car (cons (cons 1 2) (cons 3 4)))
;(cdr (cons (cons 1 2) (cons 3 4)))
;(define x (cons(cons 5 6) (cons 7 8)))
;(car (cdr x))
;(cadr x)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define setA '(1 2 3))
(define setB '(2 3 4 7))

;зад 5
;a)
(define (generate-list n)
  (if (= n 0) '()
      (cons n (generate-list (- n 1)))
      )
  )

;б)
(define (generate-list2 n)
  (define (iter list i)
    (if (= i 0) list
        (iter (cons i list)(- i 1)))
    )
  (iter '() n)
  )
   
(generate-list2 5)

;зад 6
(define (member? x l)
  (cond ((null? l) #f)
        ((eq? (car l) x) #t)
        (else (member? x (cdr l)))
        )
  )

;(member? 4 setA)
      
;зад 7
(define (intersection A B)
  (cond ((null? A) A)
        ((member? (car A) B)(cons (car A) (intersection (cdr A) B)))
        (else (intersection (cdr A) B))
        )
  )
(intersection setA setB)
