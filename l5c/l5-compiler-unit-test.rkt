#lang plai

(require rackunit "l5-compiler.rkt" "l5-parser.rkt" "../l4c/l4-format.rkt")
(require "l5-definition.rkt" "../l4c/l4-parser.rkt")

(check-equal? (find-free-vars (set) (set) (l5parse '1)) (set))
(check-equal? (find-free-vars (set) (set) (l5parse 'x)) (set 'x))
(check-equal? (find-free-vars (set) (set) (l5parse '(let ([x 1]) y))) (set 'y))
(check-equal? (find-free-vars (set) (set) (l5parse '(let ([x 1]) x))) (set))
(check-equal? (find-free-vars (set) (set) (l5parse '(let ([x x]) x))) (set 'x))
(check-equal? (find-free-vars (set) (set) (l5parse '(letrec ([x x]) x))) (set))
(check-equal? (find-free-vars (set) (set) (l5parse '(if x x x))) (set 'x))
(check-equal? (find-free-vars (set 'x) (set) (l5parse '(if x x x))) (set))
(check-equal? (find-free-vars (set 'x) (set) (l5parse '(if x y z))) (set 'y 'z))
(check-equal? (find-free-vars (set 'x) (set) (l5parse '(new-tuple x y z))) (set 'y 'z))
(check-equal? (find-free-vars (set 'x) (set) (l5parse '(new-tuple))) (set))
(check-equal? (find-free-vars (set 'x) (set) (l5parse '(begin x y))) (set 'y))
(check-equal? (find-free-vars (set) (set) (l5parse '(x y))) (set 'x 'y))
(check-equal? (find-free-vars (set 'x) (set) (l5parse '(x y))) (set 'y))
(check-equal? (find-free-vars (set) (set) (l5parse '(x))) (set 'x))
(check-equal? (find-free-vars (set 'x) (set) (l5parse '(x))) (set))
(check-equal? (find-free-vars (set 'x 'y) (set) (l5parse '(lambda (a b) x))) (set))
(check-equal? (find-free-vars (set 'x 'y) (set) (l5parse '(lambda (a b) z))) (set))
(check-equal? (find-free-vars (set 'x) (set)
                              (l5parse '(let ([f (lambda (y) z)])
                                          (f x))))
              (set))

;(displayln (format-l4-prog (l5-compiler (l5parse '(lambda (x) x)) empty)))
;(displayln (format-l4-prog (l5-compiler (l5parse '(lambda (x) y)) empty)))
;(displayln (format-l4-prog
;            (l5-compiler (l5parse '(let ([f (lambda (x) y)]) (f z))) empty)))
;(displayln (format-l4-prog
;            (l5-compiler (l5parse '(let ([f (lambda (x) (+ x y))]) (f z))) empty)))
;(displayln (format-l4-prog
;            (l5-compiler (l5parse '(let ([f +])
;                                     (let ([l (lambda (x) (f x z))]) (l y)))) empty)))
;(displayln (format-l4-prog (l5-compiler (l5parse '((lambda (x) (+ x 1)) 2)) empty)))
;(displayln (format-l4-prog (l5-compiler (l5parse '(if 1 (begin (+ x 1) (+ x 2)) 2)) empty)))
;(displayln (format-l4-prog (l5-compiler
; (l5parse '(letrec ([fib (lambda (n) (+ (fib (- n 1)) (fib (- n 2))))]) (fib 5)))
; empty)))
;(displayln (format-l4-prog (l5-compiler (l5parse '((lambda (x) x) print)) empty)))
;(displayln (format-l4-prog (l5-compiler (l5parse '(((lambda (x) x) print) 1)) empty)))
;(displayln (format-l4-prog (l5-compiler (l5parse '(print (new-array 2 3))) empty)))
;(displayln (format-l4-prog (l5-compiler
;                            (l5parse '(letrec ((loop (lambda (x)
;                                                       (begin (print x)
;                                                              (if (< x 10)
;                                                                  (loop (+ x 1))
;                                                                  1)))))
;                                        (loop 1))) empty)))
(displayln
 (format-l4-prog
  (l5-compiler
   (l5parse '(let ([f1 (lambda (f) (f 1 2))]) (f1 +))) empty)))