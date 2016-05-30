#lang plai

(require "l3-definition.rkt" "../l2c/l2-definition.rkt" "../l2c/l2-format.rkt")

(define (format-l3-value v)
  (type-case L3Value v
    [l3varia (var) (format "~a" var)]
    [l3label (lab) (format "~a" lab)]
    [l3numbe (num) (format "~a" num)]))

(define (format-pred p)
  (type-case Pred p
    [numpred () "number?"]
    [arrpred () "a?"]))

(define (format-l3-def d)
  (type-case L3Definition d
    [l3biop (oper lhs rhs)
            (format "(~a ~a ~a)"
            (cond
              [(Aop? oper) (l3-format-arop oper)]
              [(Cmp? oper) (format-comp oper)])
            (format-l3-value lhs)
            (format-l3-value rhs))]
    [l3pred (oper valu)
            (format "(~a ~a)" (format-pred oper) (format-l3-value valu))]
    [l3funcal (name argl)
              (format "~a" (cons (format-l3-value name) (map format-l3-value argl)))]
    [l3newarr (size valu)
              (format "(new-array ~a ~a)" (format-l3-value size) (format-l3-value valu))]
    [l3newtup (vall)
              (format "~a" (cons "new-tuple" (map format-l3-value vall)))]
    [l3arrref (aray posi)
              (format "(aref ~a ~a)" (format-l3-value aray) (format-l3-value posi))]
    [l3arrset (aray posi valu)
              (format "(aset ~a ~a ~a)"
                      (format-l3-value aray)
                      (format-l3-value posi)
                      (format-l3-value valu))]
    [l3arrlen (aray)
              (format "(alen ~a)" (format-l3-value aray))]
    [l3readd  ()
              (format "(read)")]
    [l3printd (valu)
              (format "(print ~a)" (format-l3-value valu))]
    [l3makecl (name vars)
              (format "(make-closure ~a ~a)" name (format-l3-value vars))]
    [l3clproc (clos)
              (format "(closure-proc ~a)" (format-l3-value clos))]
    [l3clvars (clos)
              (format "(closure-vars ~a)" (format-l3-value clos))]
    [l3value  (valu)
              (format-l3-value valu)]))

(define (format-l3-exp e)
  (type-case L3Expression e
    [l3lete (vari defi expr)
            (format "(let ((~a ~a)) ~a)"
                    (format-l3-value vari)
                    (format-l3-def defi)
                    (format-l3-exp expr))]
    [l3ife  (cond then else)
            (format "(if ~a ~a ~a)"
                    (format-l3-value cond)
                    (format-l3-exp then)
                    (format-l3-exp else))]
    [l3defe (d) (format-l3-def d)]))

(define (format-l3-func f)
  (format "(~a ~a ~a)"
          (l3func-l3fname f)
          (l3func-l3fvarl f)
          (format-l3-exp (l3func-l3fbody f))))
  
(define (format-l3-prog p)
  (format "~a" (cons (format-l3-exp (l3prog-l3entry p))
                     (map format-l3-func (l3prog-l3funcl p)))))

(define (l3-format-arop arop)
  (type-case Aop arop
    [addop () "+"]
    [subop () "-"]
    [mulop () "*"]
    [andop () "&"]))