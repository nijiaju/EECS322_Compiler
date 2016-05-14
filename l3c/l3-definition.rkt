#lang plai

(require "../l2c/l2-definition.rkt")

(define-type Pred
  [numpred]
  [arrpred])

(define-type L3Value
  [l3varia  (var is-variable?)]
  [l3label  (lab is-label?)]
  [l3numbe  (num number?)])

(define (is-label? s)
  (match (symbol->string s)
    [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*") #t]
    [_ #f]))

(define (is-variable? s)
  (match (symbol->string s)
    [(regexp #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$") #t]
    [_ #f]))

(define-type L3Definition
  [l3biop   (oper (or/c Aop? Cmp?)) (lhs L3Value?) (rhs L3Value?)]
  [l3pred   (oper Pred?) (valu L3Value?)]
  [l3funcal (name L3Value?) (argl (listof L3Value?))]
  [l3newarr (size L3Value?) (valu L3Value?)]
  [l3newtup (vall (listof L3Value?))]
  [l3arrref (aray L3Value?) (posi L3Value?)]
  [l3arrset (aray L3Value?) (posi L3Value?) (vale L3Value?)]
  [l3arrlen (aray L3Value?)]
  [l3printd (valu L3Value?)]
  [l3makecl (name is-label?) (vars L3Value?)]
  [l3clproc (clos L3Value?)]
  [l3clvars (clos L3Value?)]
  [l3value  (valu L3Value?)])

(define-type L3Expression
  [l3lete (vari L3Value?) (defi L3Definition?) (expr L3Expression?)]
  [l3ife  (cond L3Value?) (then L3Expression?) (else L3Expression?)]
  [l3defe (d L3Definition?)])

(define-struct/contract l3func
  ([l3fname is-label?]
   [l3fvarl (listof is-variable?)]
   [l3nargs number?]
   [l3fbody L3Expression?]))

(define-struct/contract l3prog
  ([l3entry L3Expression?]
   [l3funcl (listof l3func?)]))