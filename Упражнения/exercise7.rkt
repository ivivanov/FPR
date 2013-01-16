#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Упражнение 7;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define setA (list 1 2 3))
(define setB (list 2 3 4 7))

;помощни процедури;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (member? x l)
  (cond ((null? l) #f)
        ((eq? (car l) x) #t)
        (else (member? x (cdr l)))
        )
  )
;създаване на множество от списък с числа
(define (make-set A)
  (cond ((null? A) A)
        ((member? (car A) (cdr A)) (make-set (cdr A)))
        (else (cons (car A)(make-set (cdr A))))
        ))

;(make-set (list 1 2 3 3 0 0 3 2 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;Accumulate;;;;;;;;;;;;;;;;;;;;;;;;
(define (accumulate comb null_value l) 
  (if (null? l) null_value
      (comb (car l)
            (accumulate comb null_value (cdr l))
            )
      )
  )
;;;;;;;;;;;;;;;;;;; old accumulate;;;;;;;;;;;;;;;;;;;;;;;;;
(define (accum comb null_val term a next b)
  (if (> a b) null_val
      (comb (term a) (accum comb null_val term (next a) next b))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;зад 1 - Обединение на множества
(define (union A B)
  (cond ((null? A) B)
        ((member? (car A) B)(union (cdr A) B))
        (else (cons (car A) (union (cdr A) B)))
        )
  )
(union setA setB)
  
(define (union2 A B)
  (make-set (append A B)))

(union2 setA setB)

;зад 2 - Създаване на списък
;; Без accum
(define (generate-listA a b)
  (if (> a b) '()
      (cons a (generate-listA (+ a 1) b))
      )
  )
(generate-listA 1 10)

(define (generate-list a b)
  (accum 
   cons 
   '() 
   (lambda(x) x)
   a
   (lambda(x)(+ x 1))
   b
   )
  )
(generate-list 1 10)

;зад 3
;а) Merge-ване на сортирани списъци
(define (merge listA listB)
  (cond ((null? listA) listB)
        ((null? listB) listA)
        ((<= (car listA) (car listB))
             (cons (car listA) (merge (cdr listA) listB)))
        (else (cons (car listB) (merge listA (cdr listB))))
        )
  )
(merge (generate-list 3 5) (generate-list 1 7))
;b)merge sort

(define (get-odds l)
  (cond ((null? l) '())
        ((null? (cdr l))(list (car l)))
        (else (cons (car l) (get-odds (cddr l))))
        )
  )
(get-odds (generate-list 1 7))

(define (get-evens l)
  (cond ((null? l) '())
        ((null? (cdr l))l)
        (else (get-odds (cdr l)))))

(get-evens (generate-list 1 7))

(define (merge-sort l)
  (cond ((null? l) '())
        ((null? (cdr l)) l)
        (else (merge 
               (merge-sort (get-odds l)) 
               (merge-sort (get-evens l))
               )
              )
        )
  )
(merge-sort (list 1 5 1 2 8 9 134 -4))
;зад 4 
(define (composition l)
  (if (null? l)(lambda(x) x)
      (lambda(x)((car l)((composition (cdr l))x)
                        )
        )
      )
  )
;зад 5
;а)
(define (d k)
  (accum +
         0
         (lambda(x) (if (= (remainder k x) 0) 1 0))
         1
         (lambda(x) (+ x 1))
         k)
  )
(d 14)
;б)
(define (nice-pairs n)
  (accum append
         '()
         (lambda(x)(if (= (d x)(d (- n x))) (list (cons  x (- n x))) '() ))
         1
         (lambda(x)(+ x 1))
         (/ n  2)
         )
  )
(nice-pairs 50)

;зад 6
(define (begin-with? l1 l2)
  (cond ((null? l2) #t)
        ((null? l1) #f)
        ;((not (= (car l1) (car l2))) #f) 
        ((= (car l1) (car l2)) (begin-with? (cdr l1) (cdr l2)))
        (else #f))
  )

(begin-with? '(5) '(5 1))

(define (count-occurrences l1 l2)
  (cond ((null? l1) 0)
        ( (begin-with? l1 l2) (+ 1  (count-occurrences (cdr l1) l2)))
        (else  (count-occurrences (cdr l1) l2)))
  )
(count-occurrences '(0 1 1 2 3 4 5 1 1 0) '(1 1))