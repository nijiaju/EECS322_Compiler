#lang plai

(require "../l2c/l2-definition.rkt" "../l3c/l3-definition.rkt")

(define-type L4Value
  [l4var (var is-variable?)]
  [l4lab (lab is-label?)]
  [l4num (num number?)])

(define-type L4Expression
  [l4let    (vari l4var?) (valu L4Expression?) (body L4Expression?)]
  [l4if     (cond L4Expression?) (then L4Expression?) (else L4Expression?)]
  [l4biop   (oper (or/c Aop? Cmp?)) (lhs L4Expression?) (rhs L4Expression?)]
  [l4pred   (oper Pred?) (valu L4Expression?)]
  [l4funcal (name L4Expression?) (argl (listof L4Expression?))]
  [l4newarr (size L4Expression?) (valu L4Expression?)]
  [l4newtup (vall (listof L4Expression?))]
  [l4arrref (aray L4Expression?) (posi L4Expression?)]
  [l4arrset (aray L4Expression?) (posi L4Expression?) (valu L4Expression?)]
  [l4arrlen (aray L4Expression?)]
  [l4begin  (lhs  L4Expression?) (rhs  L4Expression?)]
  [l4print  (expr L4Expression?)]
  [l4makecl (name is-label?) (vars L4Expression?)]
  [l4clproc (clos L4Expression?)]
  [l4clvars (clos L4Expression?)]
  [l4value  (valu L4Value?)])

(define-struct/contract l4func
  ([l4fname is-label?]
   [l4fvarl (listof is-variable?)]
   [l4fbody L4Expression?]))

(define-struct/contract l4prog
  ([l4entry L4Expression?]
   [l4funcl (listof l4func?)]))