#lang plai

(require rackunit "l3-parser.rkt" "l3-definition.rkt" "../l2c/l2-definition.rkt")

(check-equal? (l3-parsv '3) (l3numbe 7))
(check-equal? (l3-parsv ':hello) (l3label ':hello))
(check-equal? (l3-parsv 'x) (l3varia 'x0))
(check-exn exn:fail? (lambda () (l3-parsv '2d)))
(check-equal? (l3-parsd '(+ x 5)) (l3biop (addop) (l3varia 'x1) (l3numbe 11)))
(check-equal? (l3-parsd '(a? y)) (l3pred (l3isarray) (l3varia 'y2)))
(check-equal? (l3-parsd '(func 3 5)) (l3funcal (l3varia 'func3) (list (l3numbe 7) (l3numbe 11))))
(check-equal? (l3-parsd '(make-closure :label x)) (l3makecl ':label (l3varia 'x4)))
(check-equal? (l3-parsd '(< x 5)) (l3biop (less) (l3varia 'x5) (l3numbe 11)))
;(check-equal? (l3-parsf '(:func1 (x y z) (+ x y)))
;              (l3func ':func1 (list 'x 'y 'z) 3 (l3defe (l3biop (addop) (l3varia 'x6) (l3varia 'y7)))))