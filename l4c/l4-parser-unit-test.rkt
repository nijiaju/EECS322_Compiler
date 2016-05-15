#lang plai

(require rackunit "l4-definition.rkt" "l4-parser.rkt")
(require "../l2c/l2-definition.rkt")

(check-equal?
 (l4func-l4fbody
  (l4-rename-vars-func
   (l4-parsf '(:update (is_pass results)
                       (new-tuple
                        (let [(passed (aref results 0))]
                          (if is_pass (+ 1 passed) passed))
                        (+ 1 (aref results 1)))))))
 (l4newtup
  (list
   (l4let
    (l4var 'l4_x_2)
    (l4arrref
     (l4value (l4var 'l4_x_1))
     (l4value (l4num 0)))
    (l4if
     (l4value (l4var 'l4_x_0))
     (l4biop
      (addop)
      (l4value (l4num 1))
      (l4value (l4var 'l4_x_2)))
     (l4value (l4var 'l4_x_2))))
   (l4biop
    (addop)
    (l4value (l4num 1))
    (l4arrref
     (l4value (l4var 'l4_x_1))
     (l4value (l4num 1)))))))

(check-equal?
 (car (init-context (list 'a 'b 'c 'd 'e 'f 'g) (hash)))
 '(l4_x_3 l4_x_4 l4_x_5 l4_x_6 l4_x_7 l4_x_8 l4_x_9))