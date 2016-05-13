#lang plai

(require "l3-definition.rkt" "l3-parser.rkt" "../l2c/l2-definition.rkt")

(define/contract (l3-compile-prog p)
  (-> l3prog? prog?)
  (define entry_func (label-suffix ':__MAIN__))
  (prog entry_func
        (cons (l3-compile-prog-entry (l3prog-l3entry p) entry_func)
              (map l3-compile-func (l3prog-l3funcl p)))))

(define/contract (l3-compile-prog-entry e entry_func)
  (-> L3Expression? symbol? func?)
  (type-case L3Expression e
    [l3defe (d)
            (type-case L3Definition d
              [l3funcal (name argl)
                        (let ([narg (length argl)])
                          (func entry_func 0 0
                                ;(append (l3-compile-def-call-gen-arg
                                ;         (map l3-value-l2 argl) 0)
                                ;        (if (< narg 7)
                                ;            (list (tcall (l3-value-l2 name) narg))
                                ;            (list (calli (l3-value-l2 name) narg)
                                ;                  (retun))))))]
                                (l3-compile-exp e)))]
              [else (func entry_func 0 0 (l3-compile-exp e))])]
    [else (func entry_func 0 0 (l3-compile-exp e))]))

(define/contract (l3-compile-func f)
  (-> l3func? func?)
  (func (l3func-l3fname f)
        (l3func-l3nargs f)
        0
        (append (l3-compile-def-call-get-arg (map varia (l3func-l3fvarl f)) 0)
                (l3-compile-exp (l3func-l3fbody f)))))

(define/contract (l3-compile-exp exp)
  (-> L3Expression? (listof Inst?))
  (type-case L3Expression exp
    [l3lete (vari defi expr)
            (append (l3-compile-def defi vari #f)
                    (l3-compile-exp expr))]
    [l3ife  (cond then else)
            (let ([then-label (label (label-suffix ':then_label_))]
                  [else-label (label (label-suffix ':else_label_))])
              (append (list (cjmpi (eqal) (l3-value-l2 cond) (numbr 1)
                                   else-label then-label)
                            then-label)
                      (l3-compile-exp then)
                      (list else-label)
                      (l3-compile-exp else)))]
    [l3defe (d)
            (l3-compile-def d #f #t)]))

(define/contract (l3-compile-def def var tail)
  (-> L3Definition? (or/c L3Value? boolean?) boolean? (listof Inst?))
  (define dest (if (equal? var #f) (regst 'rax) (l3-value-l2 var)))
  (type-case L3Definition def
    [l3biop (oper lhs rhs)
            (cond
              [(Aop? oper)
               (type-case Aop oper
                 [addop ()
                        (l3-compile-def-add/sub
                         oper dest (l3-value-l2 lhs) (l3-value-l2 rhs) tail)]
                 [subop ()
                        (l3-compile-def-add/sub
                         oper dest (l3-value-l2 lhs) (l3-value-l2 rhs) tail)]
                 [mulop ()
                        (l3-compile-def-mul
                         oper dest (l3-value-l2 lhs) (l3-value-l2 rhs) tail)]
                 [else (error 'l3-compile-def "l3biop-aop-unknown-operation")])]
              [(Cmp? oper)
               (l3-compile-def-cmp
                oper dest (l3-value-l2 lhs) (l3-value-l2 rhs) tail)])]
    [l3pred (oper valu)
            (l3-compile-def-pred oper dest (l3-value-l2 valu) tail)]
    [l3funcal (name argl)
              (l3-compile-def-call
               dest (l3-value-l2 name) (map l3-value-l2 argl) tail)]
    [l3newarr (size value)
              (l3-compile-def-newa dest (l3-value-l2 size) (l3-value-l2 value) tail)]
    [l3newtup (valuel)
              (l3-compile-def-newt dest (map l3-value-l2 valuel) tail)]
    [l3arrref (array posi)
              (l3-compile-def-aref dest (l3-value-l2 array) (l3-value-l2 posi) tail)]
    [l3arrset (array posi value)
              (l3-compile-def-aset
               dest (l3-value-l2 array) (l3-value-l2 posi) (l3-value-l2 value) tail)]
    [l3arrlen (array)
              (l3-compile-def-alen dest (l3-value-l2 array) tail)]
    [l3printd (value)
              (l3-compile-def-print dest (l3-value-l2 value) tail)]
    [l3makecl (name vars)
              (l3-compile-def (l3newtup (list (l3label name) vars)) var tail)]
    [l3clproc (clos)
              (l3-compile-def (l3arrref clos (l3numbe 1)) var tail)]
    [l3clvars (clos)
              (l3-compile-def (l3arrref clos (l3numbe 3)) var tail)]
    [l3value  (value)
              (append
               (list (movei dest (l3-value-l2 value)))
               (if tail (list (retun)) empty))]))

(define/contract (l3-value-l2 val)
  (-> L3Value? Inst?)
  (type-case L3Value val
    [l3varia (var) (varia var)]
    [l3label (lab) (label lab)]
    [l3numbe (num) (numbr num)]))

(define/contract (l3-compile-def-add/sub oper dest lhs rhs tail)
  (-> (or/c addop? subop?)
      (or/c numbr? regst? label? varia?)
      (or/c numbr? label? varia?)
      (or/c numbr? label? varia?)
      boolean?
      (listof Inst?))
  (append
   (list (movei dest lhs)
         (aropi oper dest rhs)
         (cond
           [(addop? oper) (aropi (subop) dest (numbr 1))]
           [(subop? oper) (aropi (addop) dest (numbr 1))]))
   (if tail (list (retun)) empty)))
  
(define/contract (l3-compile-def-mul oper dest lhs rhs tail)
  (-> mulop? (or/c numbr? regst? label? varia?)
      (or/c numbr? label? varia?) (or/c numbr? label? varia?)
      boolean?
      (listof Inst?))
  (define temp (varia (var-suffix '__tmp__)))
  (append
   (list (movei dest lhs)
         (sfopi (sfrht) dest (numbr 1))
         (movei temp rhs)
         (sfopi (sfrht) temp (numbr 1))
         (aropi oper dest temp)
         (aropi (mulop) dest (numbr 2))
         (aropi (addop) dest (numbr 1)))
   (if tail (list (retun)) empty)))

(define/contract (l3-compile-def-cmp oper dest lhs rhs tail)
  (-> Cmp? (or/c numbr? regst? label? varia?)
      (or/c numbr? label? varia?) (or/c numbr? label? varia?)
      boolean?
      (listof Inst?))
  (append
   (list (compi oper dest lhs rhs)
         (sfopi (sflft) dest (numbr 1))
         (aropi (addop) dest (numbr 1)))
   (if tail (list (retun)) empty)))

(define/contract (l3-compile-def-pred oper dest valu tail)
  (-> Pred? (or/c numbr? regst? label? varia?) (or/c numbr? label? varia?)
      boolean?
      (listof Inst?))
  (append
   (list (movei dest valu)
         (aropi (andop) dest (numbr 1))
         (if (l3isnumber? oper)
             (aropi (mulop) dest (numbr 2)) (aropi (mulop) dest (numbr -2)))
         (if (l3isnumber? oper)
             (aropi (addop) dest (numbr 1)) (aropi (addop) dest (numbr 3))))))

(define/contract (l3-compile-def-call dest name argl tail)
  (-> (or/c numbr? regst? label? varia?)
      (or/c label? varia?)
      (listof (or/c numbr? varia? label?))
      boolean?
      (listof Inst?))
  (if tail
      (l3-compile-def-tcall dest name argl)
      (l3-compile-def-fcall dest name argl)))

(define/contract (l3-compile-def-tcall dest name argl)
  (-> (or/c numbr? regst? label? varia?)
      (or/c label? varia?)
      (listof (or/c numbr? varia? label?))
      (listof Inst?))
  (if (< (length argl) 7)
      (append (l3-compile-def-call-gen-arg argl 0)
              (list (tcall name (length argl))))
      (append (l3-compile-def-fcall dest name argl)
              (list (retun)))))

(define/contract (l3-compile-def-fcall dest name argl)
  (-> (or/c numbr? regst? label? varia?)
      (or/c label? varia?)
      (listof (or/c numbr? varia? label?))
      (listof Inst?))
  (define return-label (label (label-suffix ':return_label_)))
  (append
   (list (movei (loadi (regst 'rsp) -8) return-label))
   (l3-compile-def-call-gen-arg argl 0)
   (list (calli name (length argl))
         return-label
         (movei dest (regst 'rax)))))

(define/contract (l3-compile-def-call-gen-arg argl counter)
  (-> (listof (or/c numbr? varia? label?)) number? (listof Inst?))
  (if (empty? argl)
      empty
      (cons
       (match counter
         [0 (movei (regst 'rdi) (first argl))]
         [1 (movei (regst 'rsi) (first argl))]
         [2 (movei (regst 'rdx) (first argl))]
         [3 (movei (regst 'rcx) (first argl))]
         [4 (movei (regst 'r8)  (first argl))]
         [5 (movei (regst 'r9)  (first argl))]
         [(? (λ (x) (> x 5))) (movei (loadi (regst 'rsp) (* (- counter 4) -8))
                                     (first argl))]
         [else (error 'l3-compile-def-call-gen-arg)])
       (l3-compile-def-call-gen-arg (rest argl) (+ counter 1)))))

(define/contract (l3-compile-def-call-get-arg argl counter)
  (-> (listof (or/c numbr? varia? label?)) number? (listof Inst?))
  (if (empty? argl)
      empty
      (cons
       (match counter
         [0 (movei (first argl) (regst 'rdi))]
         [1 (movei (first argl) (regst 'rsi))]
         [2 (movei (first argl) (regst 'rdx))]
         [3 (movei (first argl) (regst 'rcx))]
         [4 (movei (first argl) (regst 'r8))]
         [5 (movei (first argl) (regst 'r9))]
         [(? (λ (x) (> x 5))) (movei (first argl) (stack (* (- (length argl) 1) 8)))]
         [else (error 'l3-compile-def-call-get-arg)])
       (l3-compile-def-call-get-arg (rest argl) (+ counter 1)))))

(define/contract (l3-compile-def-newa dest size value tail)
  (-> (or/c numbr? regst? label? varia?) (or/c numbr? varia?) (or/c numbr? varia?)
      boolean?
      (listof Inst?))
  (append
   (list (movei (regst 'rdi) size)
         (movei (regst 'rsi) value)
         (caloc)
         (movei dest (regst 'rax)))
   (if tail (list (retun)) empty)))

(define/contract (l3-compile-def-newt dest valuel tail)
  (-> (or/c numbr? regst? label? varia?) (listof (or/c numbr? varia? label?))
      boolean?
      (listof Inst?))
  (define n (varia (var-suffix '__tmp__)))
  (define temp (varia (var-suffix '__tmp__)))
  (define loop (label (label-suffix ':loop_)))
  (define exit-loop (label (label-suffix ':exit_loop_)))
  (define cont-loop (label (label-suffix ':cont_loop_)))
  (append
   (list (movei (regst 'rdi) (numbr (encode (length valuel))))
         (movei (regst 'rsi) (numbr 1))
         (caloc)
         (movei dest (regst 'rax))) ;allocate array
   (l3-compile-def-newt-init dest valuel 8)
   (if tail (list (retun)) empty)))
        
(define/contract (l3-compile-def-newt-init tuple valuel offset)
  (-> (or/c numbr? regst? label? varia?) (listof (or/c numbr? varia? label?)) number?
      (listof Inst?))
  (if (cons? valuel)
      (cons (movei (loadi tuple offset) (first valuel))
            (l3-compile-def-newt-init tuple (rest valuel) (+ offset 8)))
      empty))  

(define/contract (l3-compile-def-aref dest array posi tail)
  (-> (or/c numbr? regst? label? varia?) (or/c numbr? varia?) (or/c numbr? varia?)
      boolean?
      (listof Inst?))
  (define temp (varia (var-suffix '__tmp__)))
  (define bounds-pass (label (label-suffix ':bounds_pass_)))
  (define bounds-fail (label (label-suffix ':bounds_fail_)))
  (append
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
         (movei dest (loadi dest 8)))
   (if tail (list (retun)) empty)))

(define/contract (l3-compile-def-aset dest array posi value tail)
  (-> (or/c numbr? regst? label? varia?)
      (or/c numbr? varia?)
      (or/c numbr? varia?)
      (or/c numbr? varia?)
      boolean?
      (listof Inst?))
  (define temp (varia (var-suffix '__tmp__)))
  (define bounds-pass (label (label-suffix ':bounds_pass_)))
  (define bounds-fail (label (label-suffix ':bounds_fail_)))
  (append
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
         (movei dest (numbr 1)))
   (if tail (list (retun)) empty)))

(define/contract (l3-compile-def-alen dest array tail)
  (-> (or/c numbr? regst? label? varia?) (or/c numbr? varia?)
      boolean?
      (listof Inst?))
  (append
   (list (movei dest (loadi array 0))
         (sfopi (sflft) dest (numbr 1))
         (aropi (addop) dest (numbr 1)))
   (if tail (list (retun)) empty)))

(define/contract (l3-compile-def-print dest value tail)
  (-> (or/c numbr? regst? label? varia?) (or/c numbr? varia?)
      boolean?
      (listof Inst?))
  (append
   (list (movei (regst 'rdi) value)
         (cprit)
         (movei dest (regst 'rax)))
   (if tail (list (retun)) empty)))