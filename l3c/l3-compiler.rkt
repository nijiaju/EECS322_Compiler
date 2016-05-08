#lang plai

(require "l3-definition.rkt" "l3-parser.rkt" "../l2c/l2-definition.rkt")

;(define/contract (l3-compile-exp exp)
;  (-> L3Expression? 
;  (type-case L3Expression exp
;    [l3lete (vari valu expr)

(define/contract (l3-compile-def def var tail)
  (-> L3Definition? (or/c symbol? boolean?) boolean? (listof Inst?))
  (define dest (if (equal? var #f) (regst 'rax) (varia var)))
  (type-case L3Definition def
    [l3biop (oper lhs rhs)
            (cond
              [(Aop? oper)
               (type-case Aop oper
                 [addop ()
                        (l3-compile-def-add/sub
                         oper dest (l3-value-l2 lhs) (l3-value-l2 rhs))]
                 [subop ()
                        (l3-compile-def-add/sub
                         oper dest (l3-value-l2 lhs) (l3-value-l2 rhs))]
                 [mulop ()
                        (l3-compile-def-mul
                         oper dest (l3-value-l2 lhs) (l3-value-l2 rhs))]
                 [else (error 'l3-compile-def "l3biop-aop-unknown-operation")])]
              [(Cmp? oper)
               (l3-compile-def-cmp
                oper dest (l3-value-l2 lhs) (l3-value-l2 rhs))])]
    [l3pred (oper valu)
            (l3-compile-def-pred oper dest (l3-value-l2 valu))]
    [l3newarr (size value)
              (l3-compile-def-newa dest (l3-value-l2 size) (l3-value-l2 value))]
    [l3newtup (valuel)
              (l3-compile-def-newt dest (map l3-value-l2 valuel))]
    [l3arrref (array posi)
              (l3-compile-def-aref dest (l3-value-l2 array) (l3-value-l2 posi))]
    [l3arrset (array posi value)
              (l3-compile-def-aset
               dest (l3-value-l2 array) (l3-value-l2 posi) (l3-value-l2 value))]
    [l3arrlen (array)
              (l3-compile-def-alen dest (l3-value-l2 array))]
    [l3printd (value)
              (l3-compile-def-print dest (l3-value-l2 value))]
    [l3makecl (name vars)
              (l3-compile-def (l3newtup (list (l3label name) vars)) var tail)]
    [l3clproc (clos)
              (l3-compile-def (l3arrref clos (l3numbe 1)) var tail)]
    [l3clvars (clos)
              (l3-compile-def (l3arrref clos (l3numbe 3)) var tail)]
    [l3value  (value)
              (list (movei dest (l3-value-l2 value)))]
    [else (error 'l3-compile-def "unknown l3 definition")]))

(define/contract (l3-value-l2 val)
  (-> L3Value? Inst?)
  (type-case L3Value val
    [l3varia (var) (varia var)]
    [l3label (lab) (label lab)]
    [l3numbe (num) (numbr num)]))

(define/contract (l3-compile-def-add/sub oper dest lhs rhs)
  (-> (or/c addop? subop?)
      (or/c numbr? regst? label? varia?)
      (or/c numbr? label? varia?)
      (or/c numbr? label? varia?)
      (listof Inst?))
  (list (movei dest lhs)
        (aropi oper dest rhs)
        (cond
          [(addop? oper) (aropi (subop) dest (numbr 1))]
          [(subop? oper) (aropi (addop) dest (numbr 1))])))
  
(define/contract (l3-compile-def-mul oper dest lhs rhs)
  (-> mulop? (or/c numbr? regst? label? varia?)
      (or/c numbr? label? varia?) (or/c numbr? label? varia?)
      (listof Inst?))
  (define temp (varia (var-suffix '__tmp__)))
  (list (movei dest lhs)
        (sfopi (sfrht) dest (numbr 1))
        (movei temp rhs)
        (sfopi (sfrht) temp (numbr 1))
        (aropi oper dest temp)
        (aropi (mulop) dest (numbr 2))
        (aropi (addop) dest (numbr 1))))

(define/contract (l3-compile-def-cmp oper dest lhs rhs)
  (-> Cmp? (or/c numbr? regst? label? varia?)
      (or/c numbr? label? varia?) (or/c numbr? label? varia?)
      (listof Inst?))
  (list (compi oper dest lhs rhs)
        (sfopi (sflft) dest (numbr 1))
        (aropi (addop) dest (numbr 1))))

(define/contract (l3-compile-def-pred oper dest valu)
  (-> Pred? (or/c numbr? regst? label? varia?) (or/c numbr? label? varia?)
      (listof Inst?))
  (list (movei dest valu)
        (aropi (andop) dest (numbr 1))
        (if (l3isnumber? oper)
            (aropi (mulop) dest (numbr 2)) (aropi (mulop) dest (numbr -2)))
        (if (l3isnumber? oper)
            (aropi (addop) dest (numbr 1)) (aropi (addop) dest (numbr 3)))))

(define/contract (l3-compile-def-newa dest size value)
  (-> (or/c numbr? regst? label? varia?) (or/c numbr? varia?) (or/c numbr? varia?)
      (listof Inst?))
  (list (movei (regst 'rdi) size)
        (movei (regst 'rsi) value)
        (caloc)
        (movei dest (regst 'rax))))

(define/contract (l3-compile-def-newt dest valuel)
  (-> (or/c numbr? regst? label? varia?) (listof (or/c numbr? varia? label?))
      (listof Inst?))
  (define n (varia (var-suffix '__tmp__)))
  (define temp (varia (var-suffix '__tmp__)))
  (define loop (label (label-suffix ':loop-)))
  (define exit-loop (label (label-suffix ':exit-loop-)))
  (define cont-loop (label (label-suffix ':cont-loop-)))
  (append
   (list (movei (regst 'rdi) (numbr (encode (length valuel))))
         (movei (regst 'rsi) (numbr 1))
         (caloc)
         (movei dest (regst 'rax))) ;allocate array
   (l3-compile-def-newt-init dest valuel 8)))
        
(define/contract (l3-compile-def-newt-init tuple valuel offset)
  (-> (or/c numbr? regst? label? varia?) (listof (or/c numbr? varia? label?)) number?
      (listof Inst?))
  (if (cons? valuel)
      (cons (movei (loadi tuple offset) (first valuel))
            (l3-compile-def-newt-init tuple (rest valuel) (+ offset 8)))
      empty))  

(define/contract (l3-compile-def-aref dest array posi)
  (-> (or/c numbr? regst? label? varia?) (or/c numbr? varia?) (or/c numbr? varia?)
      (listof Inst?))
  (define temp (varia (var-suffix '__tmp__)))
  (define bounds-pass (label (label-suffix ':bounds-pass-)))
  (define bounds-fail (label (label-suffix ':bounds-fail-)))
  (list (movei dest posi)
        (sfopi (sfrht) dest (numbr 1)) ;decode position
        (movei temp (loadi array 0))
        (cjmpi (less) dest temp bounds-pass bounds-fail)
        bounds-fail
        (movei (regst 'rdi) array)
        (movei (regst 'rsi) posi)
        (caerr)
        bounds-pass
        (sfopi (sflft) dest (numbr 3))
        (aropi (addop) dest array)
        (movei dest (loadi dest 8))))

(define/contract (l3-compile-def-aset dest array posi value)
  (-> (or/c numbr? regst? label? varia?)
      (or/c numbr? varia?)
      (or/c numbr? varia?)
      (or/c numbr? varia?)
      (listof Inst?))
  (define temp (varia (var-suffix '__tmp__)))
  (define bounds-pass (label (label-suffix ':bounds-pass-)))
  (define bounds-fail (label (label-suffix ':bounds-fail-)))
  (list (movei dest posi)
        (sfopi (sfrht) dest (numbr 1)) ;decode position
        (movei temp (loadi array 0))
        (cjmpi (less) dest temp bounds-pass bounds-fail)
        bounds-fail
        (movei (regst 'rdi) array)
        (movei (regst 'rsi) posi)
        (caerr)
        bounds-pass
        (sfopi (sflft) dest (numbr 3))
        (aropi (addop) dest array)
        (movei (loadi dest 8) value)
        (movei dest (numbr 1))))

(define/contract (l3-compile-def-alen dest array)
  (-> (or/c numbr? regst? label? varia?) (or/c numbr? varia?)
      (listof Inst?))
  (list (movei dest (loadi array 0))
        (sfopi (sflft) dest (numbr 1))
        (aropi (addop) dest (numbr 1))))

(define/contract (l3-compile-def-print dest value)
  (-> (or/c numbr? regst? label? varia?) (or/c numbr? varia?)
      (listof Inst?))
  (list (movei (regst 'rdi) value)
        (cprit)
        (movei dest (regst 'rax))))