#lang plai

(require "l5-compiler.rkt" "l5-parser.rkt")

(find-free-vars (set) (set) (l5parse '1))
(find-free-vars (set) (set) (l5parse 'x))
(find-free-vars (set) (set) (l5parse '(let ([x 1]) y)))
(find-free-vars (set) (set) (l5parse '(let ([x 1]) x)))
(find-free-vars (set) (set) (l5parse '(let ([x x]) x)))
(find-free-vars (set) (set) (l5parse '(letrec ([x x]) x)))
(find-free-vars (set) (set) (l5parse '(lambda (x y z) a)))
(find-free-vars (set) (set) (l5parse '(lambda (x y z) y)))
(find-free-vars (set) (set) (l5parse '(lambda (x) (lambda (y) z))))
(find-free-vars (set) (set) (l5parse '(lambda (x) (lambda (y) y))))
(find-free-vars (set) (set)
                (l5parse '(lambda (x)
                            (let ([f (lambda (y) z)])
                              (f x)))))