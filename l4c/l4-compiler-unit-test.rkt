#lang plai

(require rackunit "l4-compiler.rkt" "l4-parser.rkt" "../l3c/l3-format.rkt")
(require "../l3c/l3-definition.rkt" "../l2c/l2-definition.rkt")

(check-equal?
 (l4-normalize (l4-parse '(if (f1 (f2 a) b) (f3) (f4 (f5 c d) (:f6 e f g)))))
 (l3lete
  (l3varia 'l3_x_0)
  (l3funcal (l3varia 'f2) (list (l3varia 'a)))
  (l3lete
   (l3varia 'l3_x_1)
   (l3funcal (l3varia 'f1) (list (l3varia 'l3_x_0) (l3varia 'b)))
   (l3ife
    (l3varia 'l3_x_1)
    (l3defe (l3funcal (l3varia 'f3) '()))
    (l3lete
     (l3varia 'l3_x_2)
     (l3funcal (l3varia 'f5) (list (l3varia 'c) (l3varia 'd)))
     (l3lete
      (l3varia 'l3_x_3)
      (l3funcal
       (l3label ':f6)
       (list (l3varia 'e) (l3varia 'f) (l3varia 'g)))
      (l3defe
       (l3funcal
        (l3varia 'f4)
        (list (l3varia 'l3_x_2) (l3varia 'l3_x_3))))))))))

(check-equal?
 (l4-normalize (l4-parse '((let ([x (a b)]) (c x)) (d (let ([y e]) (f y))))))
 (l3lete
  (l3varia 'x)
  (l3funcal (l3varia 'a) (list (l3varia 'b)))
  (l3lete
   (l3varia 'l3_x_4)
   (l3funcal (l3varia 'c) (list (l3varia 'x)))
   (l3lete
    (l3varia 'y)
    (l3value (l3varia 'e))
    (l3lete
     (l3varia 'l3_x_5)
     (l3funcal (l3varia 'f) (list (l3varia 'y)))
     (l3lete
      (l3varia 'l3_x_6)
      (l3funcal (l3varia 'd) (list (l3varia 'l3_x_5)))
      (l3defe
       (l3funcal (l3varia 'l3_x_4) (list (l3varia 'l3_x_6))))))))))

(check-equal?
 (l4-normalize (l4-parse '(+ a (- b c))))
 (l3lete
  (l3varia 'l3_x_7)
  (l3biop (subop) (l3varia 'b) (l3varia 'c))
  (l3defe (l3biop (addop) (l3varia 'a) (l3varia 'l3_x_7)))))

(check-equal?
 (l3func-l3fbody
  (l4-compile-func
   (l4-parsf '(:update (is_pass results)
                       (new-tuple
                        (let [(passed (aref results 0))]
                          (if is_pass (+ 1 passed) passed))
                        (+ 1 (aref results 1)))))))
 (l3lete
  (l3varia 'passed)
  (l3arrref (l3varia 'results) (l3numbe 0))
  (l3ife
   (l3varia 'is_pass)
   (l3lete
    (l3varia 'l3_x_8)
    (l3biop (addop) (l3numbe 1) (l3varia 'passed))
    (l3lete
     (l3varia 'l3_x_9)
     (l3arrref (l3varia 'results) (l3numbe 1))
     (l3lete
      (l3varia 'l3_x_10)
      (l3biop (addop) (l3numbe 1) (l3varia 'l3_x_9))
      (l3defe
       (l3newtup (list (l3varia 'l3_x_8) (l3varia 'l3_x_10)))))))
   (l3lete
    (l3varia 'l3_x_11)
    (l3arrref (l3varia 'results) (l3numbe 1))
    (l3lete
     (l3varia 'l3_x_12)
     (l3biop (addop) (l3numbe 1) (l3varia 'l3_x_11))
     (l3defe
      (l3newtup (list (l3varia 'passed) (l3varia 'l3_x_12)))))))))

(check-equal?
 (l4-normalize (l4-parse '(begin (+ a (- b c)) (e f g))))
 (l3lete
  (l3varia 'l3_x_13)
  (l3biop (subop) (l3varia 'b) (l3varia 'c))
  (l3lete
   (l3varia 'l4_x_0)
   (l3biop (addop) (l3varia 'a) (l3varia 'l3_x_13))
   (l3defe (l3funcal (l3varia 'e) (list (l3varia 'f) (l3varia 'g)))))))

(print (format-l3-prog
        (l4-compile-prog
         (l4-parsp '((:update)
                     (:update (is_pass results)
                              (new-tuple
                               (let [(passed (aref results 0))]
                                 (if is_pass (+ 1 passed) passed))
                               (+ 1 (aref results 1))))
                     (:func (a b)
                            (a b c d e)))))))