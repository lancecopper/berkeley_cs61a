; Some utility functions that you may find useful.
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Problem 18

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  (cond ((and (not (null? list1)) (not (null? list2))) 
    (if (comp (car list1) (car list2))
      (cons (car list1) (merge comp (cdr list1) list2))
      (cons (car list2) (merge comp list1 (cdr list2)))))
    ((null? list1) list2)
    ((null? list2) list1)
    (else nil)))

(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)


;; Sort a list of lists of numbers to be in decreasing lexicographic
;; order. Relies on a correct implementation of merge.
(define (sort-lists lsts)
  (if (or (null? lsts) (null? (cdr lsts)))
      lsts
      (let ((sublsts (split lsts)))
        (merge greater-list
               (sort-lists (car sublsts))
               (sort-lists (cdr sublsts))))))

(define (greater-list x y)
  (cond ((null? y) #t)
        ((null? x) #f)
        ((> (car x) (car y)) #t)
        ((> (car y) (car x)) #f)
        (else (greater-list (cdr x) (cdr y)))))

(define (split x)
  (cond ((or (null? x) (null? (cdr x))) (cons x nil))
        (else (let ((sublsts (split (cdr (cdr x)))))
                (cons (cons (car x) (car sublsts))
                      (cons (car (cdr x)) (cdr sublsts)))))))

(merge greater-list '((3 2 1) (1 1) (0)) '((4 0) (3 2 0) (3 2) (1)))
; expect ((4 0) (3 2 1) (3 2 0) (3 2) (1 1) (1) (0))


; Problem 19

;; A list of all ways to partition TOTAL, where  each partition must
;; be at most MAX-VALUE and there are at most MAX-PIECES partitions.


(define (merge comp list1 list2)
  (cond ((and (not (null? list1)) (not (null? list2))) 
    (if (comp (car list1) (car list2))
      (cons (car list1) (merge comp (cdr list1) list2))
      (cons (car list2) (merge comp list1 (cdr list2)))))
    ((null? list1) list2)
    ((null? list2) list1)
    (else nil)))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (partitions-all x y)
  (define (partitions-merge sequence)
    (merge > (list x) sequence))
  (cond ((null? y) nil)
    ((not (list? (car y))) (if (= 0 (car y)) (list x) (merge > (list x) y)))
    (else (map partitions-merge y))))




(define (list-partitions total max-pieces max-value)
  
  ;(define (partitions-all x y)
  ;  (cond ((null? y) nil)
  ;    ((list? (car y)) (cons (partitions x (car y)) (partitions-all x (cdr y))))
  ;    (else (partitions x y))))  

  ;(define (partitions x y)
  ;  (cond ((null? y) nil)
  ;    ((= (car y) 0) (list x))
  ;    (else (merge > (list x) y))))

  (define (partitions-all x y)
  (define (partitions-merge sequence)
    (merge > (list x) sequence))
  (cond ((null? y) nil)
    ((not (list? (car y))) (if (= 0 (car y)) (list x) (merge > (list x) y)))
    (else (map partitions-merge y))))


  (define (contenate-all x y)
    (cond 
      ((and (null? x) (null? y)) nil)
      ((null? x) (if (not (list? (car y))) 
                  (cons y nil) 
                  (cons (car y) (contenate-all x (cdr y)))))
      ((null? y) (if (not (list? (car x)))
                  (cons x nil)
                  (cons (car x) (contenate-all (cdr x) y))))
      (else (if (not (list? (car x)))
                  (cons x (contenate-all nil y))
                  (cons (car x) (contenate-all (cdr x) y))))))

  (cond ((= total 0) (list 0))
    ((or (< max-pieces 1) (< max-value 1) (< total 0)) nil)
    (else (contenate-all 
      (partitions-all max-value (list-partitions (- total max-value) (- max-pieces 1) max-value))
      (list-partitions total max-pieces (- max-value 1))))))


(define (list-partitions total max-pieces max-value)
  
  (define (partitions-all x y)
    (cond ((null? y) nil)
      ((list? (car y)) (cons (partitions x (car y)) (partitions-all x (cdr y))))
      (else (partitions x y))))  

  (define (partitions x y)
    (cond ((null? y) nil)
      ((= (car y) 0) (list x))
      (else (merge > (list x) y))))

  (define (contenate-all x y)
    (cond 
      ((and (null? x) (null? y)) nil)
      ((null? x) (if (not (list? (car y))) 
                  (cons y nil) 
                  (cons (car y) (contenate-all x (cdr y)))))
      ((null? y) (if (not (list? (car x)))
                  (cons x nil)
                  (cons (car x) (contenate-all (cdr x) y))))
      (else (if (not (list? (car x)))
                  (cons x (contenate-all nil y))
                  (cons (car x) (contenate-all (cdr x) y))))))

  (cond ((= total 0) (list 0))
    ((or (< max-pieces 1) (< max-value 1) (< total 0)) nil)
    (else (contenate-all 
      (partitions-all max-value (list-partitions (- total max-value) (- max-pieces 1) max-value))
      (list-partitions total max-pieces (- max-value 1))))))


; Problem 19 tests rely on correct Problem 18.
(sort-lists (list-partitions 5 2 4))
; expect ((4 1) (3 2))
(sort-lists (list-partitions 7 3 5))
; expect ((5 2) (5 1 1) (4 3) (4 2 1) (3 3 1) (3 2 2))


; Problem 20

;; The Tree abstract data type has an entry and a list of children.
(define (make-tree entry children)
  (cons entry children))
(define (entry tree)
  (car tree))
(define (children tree)
  (cdr tree))

;; An example tree:
;;                5
;;       +--------+--------+
;;       |        |        |
;;       6        7        2
;;    +--+--+     |     +--+--+
;;    |     |     |     |     |
;;    9     8     1     6     4
;;                      |
;;                      |
;;                      3

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))


(define tree
  (make-tree 5 (list
                (make-tree 6 (list
                              (make-tree 9 nil)
                              (make-tree 8 nil)))
                (make-tree 7 (list
                              (make-tree 1 nil)))
                (make-tree 2 (list
                              (make-tree 6 (list
                                            (make-tree 3 nil)))
                              (make-tree 4 nil))))))

;; Takes a TREE of numbers and outputs a list of sums from following each
;; possible path from root to leaf.

(define (contenate x y)
  (cond
    ((and (null? x) (null? y)) nil)
    ((null? x) y)
    ((null? y) x)
    (else (cons (car x) (contenate (cdr x) y)))))

(define (contenate_children tree_list)
  (if (null? tree_list) nil
    (contenate (tree-sums (car tree_list)) (contenate_children (cdr tree_list)))))

(define (tree-sums tree)
  (define (entry_add x)
    (+ x (entry tree)))
  (if (null? (children tree)) (list (entry tree))
    (map entry_add (contenate_children (children tree)))))

(tree-sums tree)
; expect (20 19 13 16 11)
(exit)

; Problem 21 (optional)

; Draw the hax image using turtle graphics.
(define (hax d k)
  ; *** YOUR CODE HERE ***
  nil)
