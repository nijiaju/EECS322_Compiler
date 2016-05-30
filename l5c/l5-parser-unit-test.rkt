#lang plai

(require "l5-parser.rkt")

(l5parse '(+ x y))
(l5parse '(lambda (x y x) (number? (- (* x y) z))))
(l5parse '(let ([x 1]) y))
(l5parse '(print ((lambda (t) (t)) read)))
