#lang plai

(require "l3-definition.rkt" "../l2c/l2-definition.rkt")

(define (l3-parsp sexp)
  (match sexp
    [`(,e1 ,e2 ...) (l3prog (l3-parse e1) (map l3-parsf e2))]
    [_ (error 'l3-parsp "syntax error")]))

(define (l3-parsf sexp)
  (match sexp
    [`(,e1 (,e2 ...) ,e3) (l3func e1 e2 (length e2) (l3-parse e3))]
    [_ (error 'l3-parsf "syntax error")]))

(define (l3-parse sexp)
  (match sexp
    [`(let ([,e1 ,e2]) ,e3) (l3lete e1 (l3-parsd e2) (l3-parse e3))]
    [`(if ,e1 ,e2 ,e3) (l3ife (l3-parsv e1) (l3-parse e2) (l3-parse e3))]
    [_ (l3defe (l3-parsd sexp))]))

(define (l3-parsd sexp)
  (match sexp
    [`(+ ,e1 ,e2)   (l3biop (addop) (l3-parsv e1) (l3-parsv e2))]
    [`(- ,e1 ,e2)   (l3biop (subop) (l3-parsv e1) (l3-parsv e2))]
    [`(* ,e1 ,e2)   (l3biop (mulop) (l3-parsv e1) (l3-parsv e2))]
    [`(< ,e1 ,e2)   (l3biop (less) (l3-parsv e1) (l3-parsv e2))]
    [`(<= ,e1 ,e2)  (l3biop (leeq) (l3-parsv e1) (l3-parsv e2))]
    [`(= ,e1 ,e2)   (l3biop (eqal) (l3-parsv e1) (l3-parsv e2))]
    [`(number? ,e1) (l3pred (l3isnumber) (l3-parsv e1))]
    [`(a? ,e1)      (l3pred (l3isarray)  (l3-parsv e1))]
    [`(new-array ,e1 ,e2)    (l3newarr (l3-parsv e1) (l3-parsv e2))]
    [`(new-tuple ,e1 ...)    (l3newtup (map l3-parsv e1))]
    [`(aref ,e1 ,e2)         (l3arrref (l3-parsv e1) (l3-parsv e2))]
    [`(aset ,e1 ,e2 ,e3)     (l3arrset (l3-parsv e1) (l3-parsv e2) (l3-parsv e3))]
    [`(alen ,e1)             (l3arrlen (l3-parsv e1))]
    [`(print ,e1)            (l3printd (l3-parsv e1))]
    [`(make-closure ,e1 ,e2) (l3makecl e1 (l3-parsv e2))]
    [`(closure-proc ,e1)     (l3clproc (l3-parsv e1))]
    [`(closure-vars ,e1)     (l3clvars (l3-parsv e1))]
    [`(,e1 ,e2 ...) (l3funcal (l3-parsv e1) (map l3-parsv e2))]
    [`,e1           (l3value  (l3-parsv e1))]
    [_              (error 'l3-parsd "syntax error")]))

(define (l3-parsv sexp)
  (match sexp
    [(? number?)      (l3numbe (encode sexp))]
    [(? is-label?)    (l3label sexp)]
    [(? is-variable?) (l3varia (var-suffix sexp))]
    [_  (error 'l3-parsv "syntax error")]))

(define (encode n) (+ (* n 2) 1))

(define var-counter (box 0))

(define (l3-get-suffix)
  (begin0 (number->string (unbox var-counter))
          (set-box! var-counter (+ (unbox var-counter) 1))))

(define (var-suffix var)
  (string->symbol (string-append (symbol->string var) (l3-get-suffix))))