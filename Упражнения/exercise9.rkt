#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;Упражнение 9;;;;;;;;;;;;;;;;;;;;; 
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

;зад 1 Да се дефинират конструктор (make-tree root left-tree right-tree), предикати (empty-tree? tree) и (leaf? tree) и селектори (root tree), (left-tree tree) и (right-tree tree) за работа с двоични дървета. Като се използва конструкторът make-tree, да се дефинира символ tr за двоичното дърво:

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
(define tr 
  (make-tree 1
             (make-tree 2 null null)
             (make-tree 3
                        (make-tree 4
                                   (make-tree 7 null null)
                                   null)
                        (make-tree 5 null null))))

(define tre
  (make-tree 19 null null))

;зад 2
(define (sum tree)
  (if (empty-tree? tree) 0
      (+ (root tree)
         (sum (left-tree tree))
         (sum (right-tree tree))
         )
      )
  )
;(sum tr)
;зад 3
(define (tree2list tree)
  (if (empty-tree? tree) '()
      (append (list (root tree))
              (tree2list (left-tree tree))
              (tree2list (right-tree tree))
              )
      )
  )
;(tree2list tr)

;зад 4
; Да се дефинира процедура (level tree k), която по дадено двоично дърво tree намира списък от върховете на tree, които се намират на ниво k в tree (ще считаме, че коренът е на ниво 0).
(define (level k tree)
  (cond ((empty-tree? tree) '())
        ((= k 0)(list (root tree)))
        (else (append (level (- k 1) (left-tree tree))
                      (level (- k 1) (right-tree tree))
                      )
              )
        )
  )
;(level 1 tr)

;зад 5
(define (max-number-leafs2root tree)
  (if (empty-tree? tree) 0
      (+ (root tree)
         (* 10 (max (max-number-leafs2root (left-tree tree))
                    (max-number-leafs2root (right-tree tree))))))
      )

;(max-number-leafs2root tr)

;зад 6
;а)
(define (scons x ll)
  (map (lambda(i) (cons x i)) ll))

(define  (list-ways-root2leafs tree)
  (cond ((empty-tree? tree) '())
        ((and (empty-tree? (left-tree tree))
              (empty-tree? (right-tree tree))) (list (list (root tree))))
        (else (scons (root tree)
                     (append (list-ways-root2leafs (left-tree tree))
                             (list-ways-root2leafs (right-tree tree))
                             )
                     )
              )
        )
  )

;(list-ways-root2leafs tr)
;(list-ways-root2leafs tre)

;б)
(define (atom? l)
  (if (pair? l) #f #t))

;(atom? 123)
;(atom? "123")
;(atom? 'a)
;(atom? '(1 2 3))
;(atom? '())
;(atom? null)

(define (deep-reverse l)
  (if (atom? l) l
      (append (deep-reverse (cdr l))
              (list (deep-reverse (car l))))
      )
  )
;(deep-reverse '((1 3)(1 2 3)))  

(define (list-ways-leafs2root tree)
  (deep-reverse (list-ways-root2leafs tree)))

;(list-ways-leafs2root tre)
;(list-ways-leafs2root tr)

;зад 7
(define (reverse l)
  (if (null? l) l
      (append 
       (reverse (cdr l))
       (list (car l)))
      )
  )
;(reverse '(1 2 3))

(define (list2number l)
  (define (iter res pow revl)
    (if (null? revl) res
        (iter 
         (+ res (* (car revl) pow))
         (* pow 10)
         (cdr revl)
         )
        )
    )
  (iter 0 1 (reverse l))
  )
;(list2number '(1 9))

;зад 8

(define (max-number-root2leafs tree)
  (if (null? tree) 0
      (accumulate max 0 (map list2number (list-ways-root2leafs tree)))
      )
  )

;(max-number-root2leafs tr)

(define (min-number-root2leafs tree)
  (if (null? tree) 0
      (let ((l (map list2number (list-ways-root2leafs tree)))) 
        (accumulate min 
                    (car l)
                    l)
        )
      )
  )

;(min-number-root2leafs tr)