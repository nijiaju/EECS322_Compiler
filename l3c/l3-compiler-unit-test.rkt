#lang plai

(require rackunit "l3-compiler.rkt" "l3-definition.rkt" "l3-parser.rkt" "../l2c/l2-definition.rkt")

(check-equal? (l3-compile-def (l3-parsd '(+ x 3)) 'x #f)
              (list
               (movei (varia 'x) (varia 'x0))
               (aropi (addop) (varia 'x) (numbr 7))
               (aropi (subop) (varia 'x) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(< y 8)) 'x #f)
              (list
               (compi (less) (varia 'x) (varia 'y1) (numbr 17))
               (sfopi (sflft) (varia 'x) (numbr 1))
               (aropi (addop) (varia 'x) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(- x 3)) #f #f)
              (list
               (movei (regst 'rax) (varia 'x2))
               (aropi (subop) (regst 'rax) (numbr 7))
               (aropi (addop) (regst 'rax) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(* x 3)) 'y #f)
              (list
               (movei (varia 'y) (varia 'x3))
               (sfopi (sfrht) (varia 'y) (numbr 1))
               (movei (varia '__tmp__4) (numbr 7))
               (sfopi (sfrht) (varia '__tmp__4) (numbr 1))
               (aropi (mulop) (varia 'y) (varia '__tmp__4))
               (aropi (mulop) (varia 'y) (numbr 2))
               (aropi (addop) (varia 'y) (numbr 1))))