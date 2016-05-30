#lang plai

(require "l4-definition.rkt" "../l3c/l3-format.rkt" "../l2c/l2-definition.rkt" "../l2c/l2-format.rkt")

(define (format-l4-value v)
  (type-case L4Value v
    [l4var (var) (format "~a" var)]
    [l4lab (lab) (format "~a" lab)]
    [l4num (num) (format "~a" num)]))

(define (format-l4-exp e)
  (type-case L4Expression e
    [l4let (vari valu body)
           (format "(let ((~a ~a)) ~a)"
                   (format-l4-value vari)
                   (format-l4-exp valu)
                   (format-l4-exp body))]
    [l4if (cond then else)
          (format "(if ~a ~a ~a)"
                  (format-l4-exp cond)
                  (format-l4-exp then)
                  (format-l4-exp else))]
    [l4biop (oper lhs rhs)
            (format "(~a ~a ~a)"
                    (cond
                      [(Aop? oper) (l3-format-arop oper)]
                      [(Cmp? oper) (format-comp oper)])
                    (format-l4-exp lhs)
                    (format-l4-exp rhs))]
    [l4pred (oper valu)
            (format "(~a ~a)"
                    (format-pred oper)
                    (format-l4-exp valu))]
    [l4funcal (func argl)
              (cons (format-l4-exp func)
                    (map format-l4-exp argl))]
    [l4newarr (size valu)
              (format "(new-array ~a ~a)"
                      (format-l4-exp size)
                      (format-l4-exp valu))]
    [l4newtup (vall)
              (cons "new-tuple" (map format-l4-exp vall))]
    [l4arrref (aray posi)
              (format "(aref ~a ~a)"
                      (format-l4-exp aray)
                      (format-l4-exp posi))]
    [l4arrset (aray posi valu)
              (format "(aset ~a ~a ~a)"
                      (format-l4-exp aray)
                      (format-l4-exp posi)
                      (format-l4-exp valu))]
    [l4arrlen (aray)
              (format "(alen ~a)"
                      (format-l4-exp aray))]
    [l4begin (lhs rhs)
             (format "(begin ~a ~a)"
                     (format-l4-exp lhs)
                     (format-l4-exp rhs))]
    [l4read ()
            (format "(read)")]
    [l4print (expr)
             (format "(print ~a)"
                     (format-l4-exp expr))]
    [l4makecl (name vars)
              (format "(make-closure ~a ~a)"
                      name
                      (format-l4-exp vars))]
    [l4clproc (clos)
              (format "(closure-proc ~a)"
                      (format-l4-exp clos))]
    [l4clvars (clos)
              (format "(closure-vars ~a)"
                      (format-l4-exp clos))]
    [l4value (valu) (format-l4-value valu)]))

(define (format-l4-func f)
  (format "(~a ~a ~a)"
          (l4func-l4fname f)
          (l4func-l4fvarl f)
          (format-l4-exp (l4func-l4fbody f))))

(define (format-l4-prog p)
  (cons (format-l4-exp (l4prog-l4entry p))
        (map format-l4-func (l4prog-l4funcl p))))