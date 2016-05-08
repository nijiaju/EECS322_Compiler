#lang plai

(require "l3-definition.rkt" "l3-parser.rkt" "../l2c/l2-definition.rkt")

;(define/contract (l3-compile-exp exp)
;  (-> L3Expression? 
;  (type-case L3Expression exp
;    [l3lete (vari valu expr)


(define/contract (l3-compile-def def var tail)
  (-> L3Definition? (or/c symbol? boolean?) boolean? (listof Inst?))
  (type-case L3Definition def
    [l3biop (oper lhs rhs)
            (cond
              [(Aop? oper)
               (type-case Aop oper
                 [addop ()
                        (l3-compile-def-add/sub
                         oper
                         (if (equal? var #f) (regst 'rax) (varia var))
                         (l3-value-l2 lhs) (l3-value-l2 rhs))]
                 [subop ()
                        (l3-compile-def-add/sub
                         oper
                         (if (equal? var #f) (regst 'rax) (varia var))
                         (l3-value-l2 lhs) (l3-value-l2 rhs))]
                 [mulop ()
                        (l3-compile-def-mul
                         oper
                         (if (equal? var #f) (regst 'rax) (varia var))
                         (l3-value-l2 lhs) (l3-value-l2 rhs))]
                 [else (error 'l3-compile-def "l3biop-unknown-operation")])]
              [(Cmp? oper) (list (compi oper (varia var) (l3-value-l2 lhs) (l3-value-l2 rhs))
                                 (sfopi (sflft) (varia var) (numbr 1))
                                 (aropi (addop) (varia var) (numbr 1)))])]
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
  (define tmp (varia (var-suffix '__tmp__)))
  (list (movei dest lhs)
        (sfopi (sfrht) dest (numbr 1))
        (movei tmp rhs)
        (sfopi (sfrht) tmp (numbr 1))
        (aropi oper dest tmp)
        (aropi (mulop) dest (numbr 2))
        (aropi (addop) dest (numbr 1))))
