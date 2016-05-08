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

(check-equal? (l3-compile-def (l3-parsd '(a? x)) 'y #f)
              (list
               (movei (varia 'y) (varia 'x5))
               (aropi (andop) (varia 'y) (numbr 1))
               (aropi (mulop) (varia 'y) (numbr -2))
               (aropi (addop) (varia 'y) (numbr 3))))

(check-equal? (l3-compile-def (l3-parsd '(number? x)) 'y #f)
              (list
               (movei (varia 'y) (varia 'x6))
               (aropi (andop) (varia 'y) (numbr 1))
               (aropi (mulop) (varia 'y) (numbr 2))
               (aropi (addop) (varia 'y) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(alen x)) #f #f)
              (list
               (movei (regst 'rax) (loadi (varia 'x7) 0))
               (sfopi (sflft) (regst 'rax) (numbr 1))
               (aropi (addop) (regst 'rax) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(new-array x 5)) #f #f)
              (list
               (movei (regst 'rdi) (varia 'x8))
               (movei (regst 'rsi) (numbr 11))
               (caloc)
               (movei (regst 'rax) (regst 'rax))))

(check-equal? (l3-compile-def (l3-parsd '(aref x 5)) #f #f)
              (list
               (movei (regst 'rax) (numbr 11))
               (sfopi (sfrht) (regst 'rax) (numbr 1))
               (movei (varia '__tmp__10) (loadi (varia 'x9) 0))
               (cjmpi (less) (regst 'rax) (varia '__tmp__10)
                      (label ':bounds-pass-0) (label ':bounds-fail-1))
               (label ':bounds-fail-1)
               (movei (regst 'rdi) (varia 'x9))
               (movei (regst 'rsi) (numbr 11))
               (caerr)
               (label ':bounds-pass-0)
               (sfopi (sflft) (regst 'rax) (numbr 3))
               (aropi (addop) (regst 'rax) (varia 'x9))
               (movei (regst 'rax) (loadi (regst 'rax) 8))))

(check-equal? (l3-compile-def (l3-parsd '(aset a 1 5)) #f #f)
              (list
               (movei (regst 'rax) (numbr 3))
               (sfopi (sfrht) (regst 'rax) (numbr 1))
               (movei (varia '__tmp__12) (loadi (varia 'a11) 0))
               (cjmpi (less) (regst 'rax) (varia '__tmp__12)
                      (label ':bounds-pass-2) (label ':bounds-fail-3))
               (label ':bounds-fail-3)
               (movei (regst 'rdi) (varia 'a11))
               (movei (regst 'rsi) (numbr 3))
               (caerr)
               (label ':bounds-pass-2)
               (sfopi (sflft) (regst 'rax) (numbr 3))
               (aropi (addop) (regst 'rax) (varia 'a11))
               (movei (loadi (regst 'rax) 8) (numbr 11))
               (movei (regst 'rax) (numbr 1))))

(check-equal? (l3-compile-def (l3-parsd '(print 7)) #f #f)
              (list
               (movei (regst 'rdi) (numbr 15))
               (cprit)
               (movei (regst 'rax) (regst 'rax))))

(check-equal? (l3-compile-def (l3-parsd '(new-tuple 1 2 3 4 5)) 'x #f)
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

(check-equal? (l3-compile-def (l3-parsd '(make-closure :lambda x)) 'x #f)
              (list
               (movei (regst 'rdi) (numbr 5))
               (movei (regst 'rsi) (numbr 1))
               (caloc)
               (movei (varia 'x) (regst 'rax))
               (movei (loadi (varia 'x) 8) (label ':lambda))
               (movei (loadi (varia 'x) 16) (varia 'x15))))

(check-equal? (l3-compile-def (l3-parsd '(closure-proc y)) 'x #f)
              (list
               (movei (varia 'x) (numbr 1))
               (sfopi (sfrht) (varia 'x) (numbr 1))
               (movei (varia '__tmp__19) (loadi (varia 'y18) 0))
               (cjmpi (less) (varia 'x) (varia '__tmp__19)
                      (label ':bounds-pass-10) (label ':bounds-fail-11))
               (label ':bounds-fail-11)
               (movei (regst 'rdi) (varia 'y18))
               (movei (regst 'rsi) (numbr 1))
               (caerr)
               (label ':bounds-pass-10)
               (sfopi (sflft) (varia 'x) (numbr 3))
               (aropi (addop) (varia 'x) (varia 'y18))
               (movei (varia 'x) (loadi (varia 'x) 8))))

(check-equal? (l3-compile-def (l3-parsd 'y) 'x #f)
              (list (movei (varia 'x) (varia 'y20))))

(check-equal? (l3-compile-def (l3-parsd 300) 'x #f)
              (list (movei (varia 'x) (numbr 601))))

(check-equal? (l3-compile-def (l3-parsd ':hello) 'x #f)
              (list (movei (varia 'x) (label ':hello))))