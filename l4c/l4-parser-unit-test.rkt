#lang plai

(require rackunit "l4-definition.rkt" "l4-parser.rkt")
(require "../l2c/l2-definition.rkt")

(check-equal?
 (l4func-l4fbody
  (l4-parsf '(:update (is_pass results)
                      (new-tuple
                       (let [(passed (aref results 0))]
                         (if is_pass (+ 1 passed) passed))
                       (+ 1 (aref results 1))))))
 (l4newtup
  (list
   (l4let
    (l4var 'passed)
    (l4arrref
     (l4value (l4var 'results))
     (l4value (l4num 0)))
    (l4if
     (l4value (l4var 'is_pass))
     (l4biop
      (addop)
      (l4value (l4num 1))
      (l4value (l4var 'passed)))
     (l4value (l4var 'passed))))
   (l4biop
    (addop)
    (l4value (l4num 1))
    (l4arrref
     (l4value (l4var 'results))
     (l4value (l4num 1)))))))