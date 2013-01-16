#lang racket

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


;обхождане на дърво - Ляво Корен Дясно(ЛКД)
(define (lkd tree)
  (if (null? tree) '()
      (append (lkd (left-tree tree))
              (list (root tree))
              (lkd (right-tree tree))
              )
      )
  )
;(lkd tre)

(define (ler tree)
  (if (null? tree) null
      (append  (ler (left-tree tree))
               (append (list (car tree)))
                       (ler (right-tree tree)))))
;(ler tre)

;зад 1
(define (add x tree)
  (cond ((empty-tree? tree) (make-tree x '() '()))
        ((<= x (root tree)) (make-tree (root tree) 
                                       (add x (left-tree tree))
                                       (right-tree tree)))
        (else (make-tree (root tree)
                         (left-tree tree)
                         (add x (right-tree tree))))
        )
  )
;(lkd (add 6 tre))

;зад 2
(define (max-elem2 tree)
  (accumulate max 0 (lkd tree)))
;(max-elem2 tre)

;;;от упр. 
(define (max-elem tree)
  (if(empty-tree? tree) '()
     (if (empty-tree? (right-tree tree)) (root tree)
         (max-elem (right-tree tree)))))
;(max-elem tre)

;зад 3
(define (remove-max-elem tree)
  (cond ((empty-tree? tree) '())
        ((empty-tree? (right-tree tree)) (left-tree tree))
        (else
         (make-tree (root tree)
                    (left-tree tree)
                    (remove-max-elem (right-tree tree)))) 
        )
  )
;(lkd (remove-max-elem tre))

;зад 4
(define (remove x tree)
  (cond ((empty-tree? tree) '())
        ((= x (root tree))
         (if (empty-tree? (left-tree tree)) (right-tree tree)
             (make-tree (max-elem (left-tree tree))
                        (remove-max-elem (left-tree tree))
                        (right-tree tree))
             ))
        ((< x (root tree))
         (make-tree (root tree)
                    (remove x (left-tree tree))
                    (right-tree tree)))
        (else
         (make-tree (root tree)
                    (left-tree tree)
                    (remove x (right-tree tree))))
        )
  )
(lkd (remove 3 tre))

;зад 5
(define  (list2tree l)
  (if (null? l) '()
      (add (car l) (list2tree (cdr l)))
      )
  )
;(list2tree (lkd (remove 3 tre)))

(define (list2treeWithAccumulate l)
  (accumulate add
              '()
              l
              ))
;(list2treeWithAccumulate (lkd (remove 3 tre)))

;зад 6
(define  (sort-list l)
  (lkd (list2tree l)))

 (sort-list '(1 2 0 3 7 4))
 
;зад 7
 (define g
   '(graph ((2.(7 9))
            (7.(7 9))
            (9.(7))
            (8.())
            )))
 
(define  (vertices g)
  (map car (cdr g)))
 
(vertices g)

;зад 8
(define (member? x l)
  (cond ((null? l) #f)
        ((eq? (car l) x) #t)
        (else (member? x (cdr l)))
        )
  )

(define (union-lists A B)
  (cond ((null? A) B)
        ((member? (car A) B)(union-lists (cdr A) B))
        (else (cons (car A) (union-lists (cdr A) B)))
        )
  )
;;;;тази задача нещо грешно е преписана и не мога да дебъгна 
;(define (union-graphs g1 g2)
;  (cons "graph"
;        (map (lambda(x)(cons x(union-lists (if (not (null? (assq x (cdr g1))))(cdr (assq x (cdr g1)))) g2))
;               (union-lists (vertices g1)
;                            (vertices g2))))))