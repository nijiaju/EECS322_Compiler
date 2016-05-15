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
  [l4funcal (func L4Expression?) (argl (listof L4Expression?))]
  [l4newarr (size L4Expression?) (valu L4Expression?)]
  [l4newtup (vall (listof L4Expression?))]
  [l4arrref (aray L4Expression?) (posi L4Expression?)]
  [l4arrset (aray L4Expression?) (posi L4Expression?) (valu L4Expression?)]
  [l4arrlen (aray L4Expression?)]
;  [l4begin  (lhs  L4Expression?) (rhs  L4Expression?)]
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

(define-type L4Context
  [l4letctxt (var l4var?) (body L4Expression?) (k L4Context?)]
  [l4ifctxt  (then L4Expression?) (else L4Expression?) (k L4Context?)]
  [l4bilctxt (oper (or/c Aop? Cmp?)) (rhs L4Expression?) (k L4Context?)]
  [l4birctxt (oper (or/c Aop? Cmp?)) (lhs L3Value?) (k L4Context?)]
  [l4prdctxt (oper Pred?) (k L4Context?)]
  [l4functxt (argl (listof L4Expression?)) (k L4Context?)]
  [l4argctxt (func L3Value?) (done (listof L3Value?))
             (left (listof L4Expression?)) (k L4Context?)]
  [l4nasctxt (valu L4Expression?) (k L4Context?)]
  [l4navctxt (size L3Value?) (k L4Context?)]
  [l4ntpctxt (done (listof L3Value?)) (left (listof L4Expression?)) (k L4Context?)]
  [l4aractxt (posi L4Expression?) (k L4Context?)]
  [l4arpctxt (aray L3Value?) (k L4Context?)]
  [l4asactxt (posi L4Expression?) (valu L4Expression?) (k L4Context?)]
  [l4aspctxt (aray L3Value?) (valu L4Expression?) (k L4Context?)]
  [l4asvctxt (aray L3Value?) (posi L3Value?) (k L4Context?)]
  [l4alnctxt (k L4Context?)]
  [l4prtctxt (k L4Context?)]
  [l4mclctxt (labl is-label?) (k L4Context?)]
  [l4clpctxt (k L4Context?)]
  [l4clvctxt (k L4Context?)]
  [l4noctxt])

(define/contract (l4v-to-l3v v)
  (-> L4Value? L3Value?)
  (type-case L4Value v
    [l4var (var) (l3varia var)]
    [l4lab (lab) (l3label lab)]
    [l4num (num) (l3numbe num)]))