#lang racket
;;;;;;;;;;;;;;;;;;;;Домашно 4;;;;;;;;;;;;;;;;;;;;;
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
;конструктор
(define (make-tree root left-tree right-tree)
  (list root left-tree right-tree))

;селектори
(define (root tree)
  (car tree))

(define (left-tree tree)
  (cadr tree))

(define (right-tree tree)
  (caddr tree))


;предикати
(define (empty-tree? tree)
  (null? tree))
  
(define (leaf? tree)
  (and (null? (left-tree tree))
       (null? (right-tree tree))))
  
;символи
(define tre
  (make-tree 3 
             (make-tree 1
                        (make-tree 0 null null)
                        (make-tree 2 null null))
             (make-tree 7             
                        (make-tree 4 null null)
                        null)
             )
  )


;зад 1 
(define (remove-min-elem tree)
  (cond ((empty-tree? tree) '())
        ((empty-tree? (left-tree tree)) (right-tree tree))
        (else
         (make-tree (root tree)
                    (right-tree tree)
                    (remove-min-elem (left-tree tree)))) 
        )
  )
(remove-min-elem tre)