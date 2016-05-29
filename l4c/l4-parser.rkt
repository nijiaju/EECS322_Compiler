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
    ;[`(begin ,e1 ,e2)        (l4let (l4var (var-suffix 'l4_x_ 'L4))
    ;                                (l4-parse e1) (l4-parse e2))]
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

(define/contract (l4-rename-vars p)
  (-> l4prog? l4prog?)
  (l4prog (l4-rename-vars-exp (l4prog-l4entry p) (hash))
          (map l4-rename-vars-func (l4prog-l4funcl p))))

(define/contract (l4-rename-vars-func f)
  (-> l4func? l4func?)
  (define v/c (init-context (l4func-l4fvarl f) (hash)))
  (l4func (l4func-l4fname f)
          (car v/c)
          (l4-rename-vars-exp (l4func-l4fbody f) (cdr v/c))))

(define/contract (l4-rename-vars-exp e c)
  (-> L4Expression? hash? L4Expression?)
  (type-case L4Expression e
    [l4let    (vari valu body)
              (let ([c2 (hash-set c (l4-unwrap-var vari) (var-suffix 'l4_x_ 'L4))])
               (l4let (l4var (hash-ref c2 (l4-unwrap-var vari)))
                      (l4-rename-vars-exp valu c)
                      (l4-rename-vars-exp body c2)))]
    [l4if     (cond then else)
              (l4if (l4-rename-vars-exp cond c)
                   (l4-rename-vars-exp then c)
                   (l4-rename-vars-exp else c))]
    [l4biop   (oper lhs rhs)
              (l4biop oper
                     (l4-rename-vars-exp lhs c)
                     (l4-rename-vars-exp rhs c))]
    [l4pred   (oper valu)
              (l4pred oper (l4-rename-vars-exp valu c))]
    [l4funcal (name argl)
              (l4funcal (l4-rename-vars-exp name c)
                        (map (λ (x) (l4-rename-vars-exp x c)) argl))]
    [l4newarr (size valu)
              (l4newarr (l4-rename-vars-exp size c)
                        (l4-rename-vars-exp valu c))]
    [l4newtup (vall)
              (l4newtup (map (λ (x) (l4-rename-vars-exp x c)) vall))]
    [l4arrref (aray posi)
              (l4arrref (l4-rename-vars-exp aray c)
                        (l4-rename-vars-exp posi c))]
    [l4arrset (aray posi valu)
              (l4arrset (l4-rename-vars-exp aray c)
                        (l4-rename-vars-exp posi c)
                        (l4-rename-vars-exp valu c))]
    [l4arrlen (aray)
              (l4arrlen (l4-rename-vars-exp aray c))]
    [l4begin  (lhs rhs)
              (l4begin  (l4-rename-vars-exp lhs  c)
                        (l4-rename-vars-exp rhs  c))]
    [l4print  (valu)
              (l4print (l4-rename-vars-exp valu c))]
    [l4makecl (name vars)
              (l4makecl name (l4-rename-vars-exp vars c))]
    [l4clproc (clos)
              (l4clproc (l4-rename-vars-exp clos c))]
    [l4clvars (clos)
              (l4clvars (l4-rename-vars-exp clos c))]
    [l4value  (valu)
              (l4value  (l4-rename-variable valu c))]))
                        
               

(define/contract (l4-rename-variable v c)
  (-> L4Value? hash? L4Value?)
  (type-case L4Value v
    [l4var (var) (l4var (hash-ref c var))]
    [else v]))

(define/contract (init-context argl h)
  (-> (listof is-variable?) hash? pair?)
  (cond
    [(empty? argl) (cons empty h)]
    [else (let ([new-var (var-suffix 'l4_x_ 'L4)]
                [res (init-context (rest argl) h)])
            (cons (cons new-var (car res))
                  (hash-set (cdr res) (first argl) new-var)))]))

(define/contract (l4-unwrap-var v)
  (-> L4Value? is-variable?)
  (type-case L4Value v
    [l4var (var) var]
    [else (error 'l4-unwrap-var)]))