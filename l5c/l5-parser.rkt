#lang plai

(require "l5-definition.rkt")

(define (l5parse sexp)
  (match sexp
    [`(lambda (,x ...) ,e) (l5lambda (map l5parse x) (l5parse e))]
    [`(let ([,x ,e1]) ,e2) (l5let (l5parse x) (l5parse e1) (l5parse e2))]
    [`(letrec ([,x ,e1]) ,e2) (l5letrec (l5parse x) (l5parse e1) (l5parse e2))]
    [`(if ,e1 ,e2 ,e3) (l5if (l5parse e1) (l5parse e2) (l5parse e3))]
    [`(new-tuple ,e ...) (l5newtup (map l5parse e))]
    [`(begin ,e1 ,e2) (l5begin (l5parse e1) (l5parse e2))]
    [`(,e1 ,e2 ...) (l5call (l5parse e1) (map l5parse e2))]
    [(? number?) (l5num sexp)]
    [(? is-l5prim) (l5prim sexp)]
    [(? is-l5var) (l5var sexp)]
    [_ (error 'l5parse "Syntax Error @ ~a" sexp)]))