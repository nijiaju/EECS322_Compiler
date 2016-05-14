#lang plai

(require "l4-definition.rkt" "../l3c/l3-definition.rkt"
         "../l3c/l3-parser.rkt" "../l2c/l2-definition.rkt")

(define (l4-parsp sexp)
  (match sexp
    [`(,e1 ,e2 ...) (l4prog (l4-parse e1) (map l4-parsf e2))]
    [_ (error 'l4-parsp "syntax error")]))

(define (l4-parsf sexp)
  (match sexp
    [`(,e1 (,e2 ...) ,e3) (l4func e1 e2 (l4-parse e3))]
    [_ (error 'l4-parsf "sytax error")]))

(define (l4-parse sexp)
  (match sexp
    [`(let ([,e1 ,e2]) ,e3) (l4let (l4-parsv e1) (l4-parse e2) (l4-parse e3))]
    [`(if ,e1 ,e2 ,e3)      (l4if  (l4-parse e1) (l4-parse e2) (l4-parse e3))]
    [`(+ ,e1 ,e2)   (l4biop (addop) (l4-parse e1) (l4-parse e2))]
    [`(- ,e1 ,e2)   (l4biop (subop) (l4-parse e1) (l4-parse e2))]
    [`(* ,e1 ,e2)   (l4biop (mulop) (l4-parse e1) (l4-parse e2))]
    [`(< ,e1 ,e2)   (l4biop (less)  (l4-parse e1) (l4-parse e2))]
    [`(<= ,e1 ,e2)  (l4biop (leeq)  (l4-parse e1) (l4-parse e2))]
    [`(= ,e1 ,e2)   (l4biop (eqal)  (l4-parse e1) (l4-parse e2))]
    [`(number? ,e1) (l4pred (numpred) (l4-parse e1))]
    [`(a? ,e1)      (l4pred (arrpred) (l4-parse e1))]
    [`(new-array ,e1 ,e2)    (l4newarr (l4-parse e1) (l4-parse e2))]
    [`(new-tuple ,e1 ...)    (l4newtup (map l4-parse e1))]
    [`(aref ,e1 ,e2)         (l4arrref (l4-parse e1) (l4-parse e2))]
    [`(aset ,e1 ,e2 ,e3)     (l4arrset (l4-parse e1) (l4-parse e2) (l4-parse e3))]
    [`(alen ,e1)             (l4arrlen (l4-parse e1))]
    [`(begin ,e1 ,e2)        (l4begin  (l4-parse e1) (l4-parse e2))]
    [`(print ,e1)            (l4print  (l4-parse e1))]
    [`(make-closure ,e1 ,e2) (l4makecl e1 (l4-parse e2))]
    [`(closure-proc ,e1)     (l4clproc (l4-parse e1))]
    [`(closure-vars ,e1)     (l4clvars (l4-parse e1))]
    [`(,e1 ,e2 ...)          (l4funcal (l4-parse e1) (map l4-parse e2))]
    [(? number?)      (l4value (l4num sexp))]
    [(? is-label?)    (l4value (l4lab sexp))]
    [(? is-variable?) (l4value (l4var sexp))]))
    

(define (l4-parsv sexp)
  (match sexp
    [(? number?)      (l4num sexp)]
    [(? is-label?)    (l4lab sexp)]
    [(? is-variable?) (l4var sexp)]
    [_ (error 'l4-parsv "syntax error")]))