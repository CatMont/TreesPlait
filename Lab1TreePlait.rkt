#lang plait

(define-type Tree

    (leaf [val : Number])

    (node [val : Number]

          [left : Tree]

          [right : Tree]))
; Part 1 Sum
(define (sum t)
  (type-case Tree t
    [(leaf v) (+ v 0)]
    [(node v l r) (+ v
                     (+(sum l)
                     (sum r)))]))

(sum (node 4 (leaf 3) (leaf 2)))


(test (sum (leaf 3))
      3)

(test (sum (node 5 (leaf 6) (leaf 7)))
      18)

(test (sum (node 5 (node 1 (leaf 2) (leaf 3)) (node 4 (leaf 3) (leaf 2))))
      20)

;Part 2 Negate

(define (negate t)
  (type-case Tree t
    [(leaf v) (leaf (- 0 v))]
    [(node v l r) ( node (- 0 v) (negate l) (negate r) )]))


(test (negate (node 5 (leaf 6) (leaf 7)))
      (node -5 (leaf -6) (leaf -7)))

(test (negate (leaf 3))
      (leaf -3))


;Part 3 Contains?

(define (contains? t a )
  (type-case Tree t
    [(leaf v) (if (equal? v a)
                  '#t
                  '#f)]
    [(node v l r) (cond
                    [(= v a) '#t]
                    [(not(= v a)) (cond
                                    [(equal? (contains? l a) '#t) '#t]
                                    [else (contains? r a)])])]))

(test (contains? (node 4 (leaf 3) (leaf 2)) 2)
      '#t)
(test (contains? (node 4 (leaf 3) (leaf 2)) 1)
      '#f)





;Part 4 determines if leaves are bigger than their nodes


(define (bigger-leaves? [tr : Tree] [acc : Number] [out : (Listof Number)])
  (type-case Tree tr 
    ([node v l r] (cond
                    [(> v acc) (append out
                                (append (bigger-leaves? l (+ v acc) out)
                                        (bigger-leaves? r (+ v acc) out)))]
                    [else '(0)]))                                                     
    ([leaf v]  (cond
                 [(> v acc)(list (+ acc v))]
                 [else '(0)]))))


(define (big-leaves? [tr : Tree]) 
  (cond
    [(member 0(bigger-leaves? tr 0 '())) '#f]
    [else '#t]))



(test (big-leaves? (node 1 (node 2 (leaf 4) (leaf 5)) (node 2 (leaf 4) (leaf 4))))
      '#t)
(test (big-leaves? (node 2 (leaf 1) (leaf 2)))
      '#f)

;Part 5 positive-trees? determines if elements return positive value when summed

(define L '())

(define (lister t)
  (type-case Tree t
    [(leaf v) (append (list v) L)]
    [(node v l r) (append (list v) (append (lister l) (lister r)))]))

(define (summer lst)
  (if (empty? lst) 0
      (+ (first lst) (summer (rest lst)))))

(define (positive-tree? t)
  (cond
    [(= t 0) '#t]
    [(> t 0) '#t]
    [else '#f]))




                  
(test (positive-tree?(summer(lister (node 3 (node 2 (leaf 2) (leaf 1))  (node 4 (leaf 2) (leaf 3))))))
      '#t)
(test (positive-tree?(summer(lister (node 2 (leaf -14) (leaf 1)))))
      '#f)
                     
      
