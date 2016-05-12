#lang plai

(require rackunit "l3-compiler.rkt" "l3-definition.rkt" "l3-parser.rkt"
         "../l2c/l2-definition.rkt" "../l2c/l2-format.rkt")

(check-equal? (l3-compile-def (l3-parsd '(+ x 3)) #f #t)
              (list
               (movei (regst 'rax) (varia 'x))
               (aropi (addop) (regst 'rax) (numbr 7))
               (aropi (subop) (regst 'rax) (numbr 1))
               (retun)))

(check-equal? (l3-compile-def (l3-parsd '(< y 8)) (l3varia 'x) #f)
              (list
               (compi (less) (varia 'x) (varia 'y) (numbr 17))
               (sfopi (sflft) (varia 'x) (numbr 1))
               (aropi (addop) (varia 'x) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(- x 3)) #f #f)
              (list
               (movei (regst 'rax) (varia 'x))
               (aropi (subop) (regst 'rax) (numbr 7))
               (aropi (addop) (regst 'rax) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(* x 3)) (l3varia 'y) #f)
              (list
               (movei (varia 'y) (varia 'x))
               (sfopi (sfrht) (varia 'y) (numbr 1))
               (movei (varia '__tmp__0) (numbr 7))
               (sfopi (sfrht) (varia '__tmp__0) (numbr 1))
               (aropi (mulop) (varia 'y) (varia '__tmp__0))
               (aropi (mulop) (varia 'y) (numbr 2))
               (aropi (addop) (varia 'y) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(a? x)) (l3varia 'y) #f)
              (list
               (movei (varia 'y) (varia 'x))
               (aropi (andop) (varia 'y) (numbr 1))
               (aropi (mulop) (varia 'y) (numbr -2))
               (aropi (addop) (varia 'y) (numbr 3))))

(check-equal? (l3-compile-def (l3-parsd '(number? x)) (l3varia 'y) #f)
              (list
               (movei (varia 'y) (varia 'x))
               (aropi (andop) (varia 'y) (numbr 1))
               (aropi (mulop) (varia 'y) (numbr 2))
               (aropi (addop) (varia 'y) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(alen x)) #f #f)
              (list
               (movei (regst 'rax) (loadi (varia 'x) 0))
               (sfopi (sflft) (regst 'rax) (numbr 1))
               (aropi (addop) (regst 'rax) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(new-array x 5)) #f #f)
              (list
               (movei (regst 'rdi) (varia 'x))
               (movei (regst 'rsi) (numbr 11))
               (caloc)
               (movei (regst 'rax) (regst 'rax))))

(check-equal? (l3-compile-def (l3-parsd '(aref x 5)) #f #f)
              (list
               (movei (regst 'rax) (numbr 11))
               (sfopi (sfrht) (regst 'rax) (numbr 1))
               (movei (varia '__tmp__1) (loadi (varia 'x) 0))
               (cjmpi (less) (regst 'rax) (varia '__tmp__1)
                      (label ':bounds-pass-0) (label ':bounds-fail-1))
               (label ':bounds-fail-1)
               (movei (regst 'rdi) (varia 'x))
               (movei (regst 'rsi) (numbr 11))
               (caerr)
               (label ':bounds-pass-0)
               (sfopi (sflft) (regst 'rax) (numbr 3))
               (aropi (addop) (regst 'rax) (varia 'x))
               (movei (regst 'rax) (loadi (regst 'rax) 8))))

(check-equal? (l3-compile-def (l3-parsd '(aset a 1 5)) #f #f)
              (list
               (movei (regst 'rax) (numbr 3))
               (sfopi (sfrht) (regst 'rax) (numbr 1))
               (movei (varia '__tmp__2) (loadi (varia 'a) 0))
               (cjmpi (less) (regst 'rax) (varia '__tmp__2)
                      (label ':bounds-pass-2) (label ':bounds-fail-3))
               (label ':bounds-fail-3)
               (movei (regst 'rdi) (varia 'a))
               (movei (regst 'rsi) (numbr 3))
               (caerr)
               (label ':bounds-pass-2)
               (sfopi (sflft) (regst 'rax) (numbr 3))
               (aropi (addop) (regst 'rax) (varia 'a))
               (movei (loadi (regst 'rax) 8) (numbr 11))
               (movei (regst 'rax) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(print 7)) #f #f)
              (list
               (movei (regst 'rdi) (numbr 15))
               (cprit)
               (movei (regst 'rax) (regst 'rax))))

(check-equal? (l3-compile-def (l3-parsd '(new-tuple 1 2 3 4 5)) (l3varia 'x) #f)
              (list
               (movei (regst 'rdi) (numbr 11))
               (movei (regst 'rsi) (numbr 1))
               (caloc)
               (movei (varia 'x) (regst 'rax))
               (movei (loadi (varia 'x) 8) (numbr 3))
               (movei (loadi (varia 'x) 16) (numbr 5))
               (movei (loadi (varia 'x) 24) (numbr 7))
               (movei (loadi (varia 'x) 32) (numbr 9))
               (movei (loadi (varia 'x) 40) (numbr 11))))

(check-equal? (l3-compile-def (l3-parsd '(make-closure :lambda x)) (l3varia 'x) #f)
              (list
               (movei (regst 'rdi) (numbr 5))
               (movei (regst 'rsi) (numbr 1))
               (caloc)
               (movei (varia 'x) (regst 'rax))
               (movei (loadi (varia 'x) 8) (label ':lambda))
               (movei (loadi (varia 'x) 16) (varia 'x))))

(check-equal? (l3-compile-def (l3-parsd '(closure-proc y)) (l3varia 'x) #f)
              (list
               (movei (varia 'x) (numbr 1))
               (sfopi (sfrht) (varia 'x) (numbr 1))
               (movei (varia '__tmp__7) (loadi (varia 'y) 0))
               (cjmpi (less) (varia 'x) (varia '__tmp__7)
                      (label ':bounds-pass-10) (label ':bounds-fail-11))
               (label ':bounds-fail-11)
               (movei (regst 'rdi) (varia 'y))
               (movei (regst 'rsi) (numbr 1))
               (caerr)
               (label ':bounds-pass-10)
               (sfopi (sflft) (varia 'x) (numbr 3))
               (aropi (addop) (varia 'x) (varia 'y))
               (movei (varia 'x) (loadi (varia 'x) 8))))

(check-equal? (l3-compile-def (l3-parsd 'y) (l3varia 'x) #f)
              (list (movei (varia 'x) (varia 'y))))

(check-equal? (l3-compile-def (l3-parsd 300) (l3varia 'x) #f)
              (list (movei (varia 'x) (numbr 601))))

(check-equal? (l3-compile-def (l3-parsd ':hello) (l3varia 'x) #f)
              (list (movei (varia 'x) (label ':hello))))

(check-equal? (l3-compile-def-call-gen-arg (list (varia 'a) (varia 'b)) 0)
              (list (movei (regst 'rdi) (varia 'a)) (movei (regst 'rsi) (varia 'b))))

(check-equal? (l3-compile-def-call-gen-arg (list (varia 'a) (varia 'b) (varia 'c) (varia 'd)
                                                 (varia 'e) (varia 'f) (varia 'g) (varia 'h)) 0)
              (list
               (movei (regst 'rdi) (varia 'a)) (movei (regst 'rsi) (varia 'b))
               (movei (regst 'rdx) (varia 'c)) (movei (regst 'rcx) (varia 'd))
               (movei (regst 'r8) (varia 'e)) (movei (regst 'r9) (varia 'f))
               (movei (loadi (regst 'rsp) -16) (varia 'g))
               (movei (loadi (regst 'rsp) -24) (varia 'h))))

(check-equal? (l3-compile-def (l3-parsd '(:func a b)) #f #t)
              (list
               (movei (regst 'rdi) (varia 'a))
               (movei (regst 'rsi) (varia 'b))
               (tcall (label ':func) 2)))

(check-equal? (l3-compile-def (l3-parsd '(:func a b c d e f g h)) #f #t)
              (list
               (movei (loadi (regst 'rsp) -8) (label ':return-label-12))
               (movei (regst 'rdi) (varia 'a))
               (movei (regst 'rsi) (varia 'b))
               (movei (regst 'rdx) (varia 'c))
               (movei (regst 'rcx) (varia 'd))
               (movei (regst 'r8) (varia 'e))
               (movei (regst 'r9) (varia 'f))
               (movei (loadi (regst 'rsp) -16) (varia 'g))
               (movei (loadi (regst 'rsp) -24) (varia 'h))
               (calli (label ':func) 8)
               (label ':return-label-12)
               (movei (regst 'rax) (regst 'rax))))

(check-equal? (l3-compile-def (l3-parsd '(:func a b c d e f g h)) (l3varia 'x) #f)
              (list
               (movei (loadi (regst 'rsp) -8) (label ':return-label-13))
               (movei (regst 'rdi) (varia 'a))
               (movei (regst 'rsi) (varia 'b))
               (movei (regst 'rdx) (varia 'c))
               (movei (regst 'rcx) (varia 'd))
               (movei (regst 'r8) (varia 'e))
               (movei (regst 'r9) (varia 'f))
               (movei (loadi (regst 'rsp) -16) (varia 'g))
               (movei (loadi (regst 'rsp) -24) (varia 'h))
               (calli (label ':func) 8)
               (label ':return-label-13)
               (movei (varia 'x) (regst 'rax))))

(check-equal? (l3-compile-def-call-get-arg (list (varia 'a) (varia 'b)) 0)
              (list (movei (varia 'a) (regst 'rdi)) (movei (varia 'b) (regst 'rsi))))

(check-equal? (l3-compile-def-call-get-arg
               (list (varia 'a) (varia 'b) (varia 'c) (varia 'd)
                     (varia 'e) (varia 'f) (varia 'g) (varia 'h) (varia 'i)) 0)
              (list
               (movei (varia 'a) (regst 'rdi))
               (movei (varia 'b) (regst 'rsi))
               (movei (varia 'c) (regst 'rdx))
               (movei (varia 'd) (regst 'rcx))
               (movei (varia 'e) (regst 'r8))
               (movei (varia 'f) (regst 'r9))
               (movei (varia 'g) (stack 16))
               (movei (varia 'h) (stack 8))
               (movei (varia 'i) (stack 0))))

(check-equal? (l3-compile-exp (l3-parse '(let ([x 1]) (+ x 2))))
              (list
               (movei (varia 'x) (numbr 3))
               (movei (regst 'rax) (varia 'x))
               (aropi (addop) (regst 'rax) (numbr 5))
               (aropi (subop) (regst 'rax) (numbr 1))
               (retun)))

#|
(check-equal? (l3-compile-func (l3-parsf '(:func (a b c d e f g h) (+ a b))))
              (func ':func 8 0
                    (list
                     (movei (varia 'a) (regst 'rdi))
                     (movei (varia 'b) (regst 'rsi))
                     (movei (varia 'c) (regst 'rdx))
                     (movei (varia 'd) (regst 'rcx))
                     (movei (varia 'e) (regst 'r8))
                     (movei (varia 'f) (regst 'r9))
                     (movei (varia 'g) (stack 8))
                     (movei (varia 'h) (stack 0))
                     (movei (regst 'rax) (varia 'a))
                     (aropi (addop) (regst 'rax) (varia 'b))
                     (aropi (subop) (regst 'rax) (numbr 1))
                     (retun))))
|#
(format-func (l3-compile-func (l3-parsf '(:func (a b c d e f g h) (+ a b)))))
(format-func (l3-compile-prog-entry (l3-parse '(:main 1 2 3 4 5)) ':__MAIN__))