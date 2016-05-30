#lang plai

(require "l5-definition.rkt" "../l4c/l4-definition.rkt" "../l3c/l3-parser.rkt")
(require "l5-parser.rkt" "../l2c/l2-definition.rkt" "../l3c/l3-definition.rkt")

(define/contract (l5-compiler e funcs)
  (-> L5e? (listof l4func?) l4prog?)
  (type-case L5e e
    [l5lambda (vars body) (l5-compiler-lambda vars body funcs)]
    [l5let (var value body) (l5-compiler-let var value body funcs)]
    [l5letrec (var value body) (l5-compiler-letrec var value body funcs)]
    [l5if (cond then else) (l5-compiler-if cond then else funcs)]
    [l5newtup (vals) (l5-compiler-newtup vals funcs)]
    [l5begin (exp1 exp2) (l5-compiler-begin exp1 exp2 funcs)]
    [l5call (func args) (l5-compiler-call func args funcs)]
    [l5var (var) (l4prog (l4value (l4var var)) funcs)]
    [l5num (num) (l4prog (l4value (l4num num)) funcs)]
    [l5prim (prim) (l5-compiler-prim prim funcs)]))

(define/contract (l5-compiler-lambda vars body funcs)
  (-> (listof l5var?) L5e? (listof l4func?) l4prog?)
  (define free-vars (find-free-vars (list->set (map unwrap-l5var vars)) (set) body))
  (define func-vars (map l4value (map l4var (set->list free-vars))))
  (define func-name (label-suffix ':l5_lambda_ 'L5))
  (define cmpd-body (l5-compiler body empty))
  (define vars-tup  (var-suffix 'l5_vars_tup_ 'L5))
  (l4prog (l4makecl func-name (l4newtup func-vars))
          (cons (l4func func-name
                        (cons vars-tup (map unwrap-l5var vars))
                        (foldl (λ (v c e)
                                 (l4let (l4var v)
                                        (l4arrref (l4value (l4var vars-tup))
                                                  (l4value (l4num c)))
                                        e))
                               (l4prog-l4entry cmpd-body)
                               (set->list free-vars)
                               (range (length (set->list free-vars)))))
                (append funcs (l4prog-l4funcl cmpd-body)))))

(define/contract (l5-compiler-call func args funcs)
  (-> L5e? (listof L5e?) (listof l4func?) l4prog?)
  (if (l5prim? func)
      (l5-compiler-call-prim func args funcs)
      (l5-compiler-call-clos func args funcs)))

(define/contract (l5-compiler-call-prim func args funcs)
  (-> l5prim? (listof L5e?) (listof l4func?) l4prog?)
  (define res-args (map (λ (a) (l5-compiler a empty)) args))
  (type-case L5e func
    [l5prim (prim)
            (l4prog
             (match prim
               ['+ (l4biop (addop)
                           (l4prog-l4entry (first res-args))
                           (l4prog-l4entry (second res-args)))]
               ['- (l4biop (subop)
                           (l4prog-l4entry (first res-args))
                           (l4prog-l4entry (second res-args)))]
               ['* (l4biop (mulop)
                           (l4prog-l4entry (first res-args))
                           (l4prog-l4entry (second res-args)))]
               ['< (l4biop (less)
                           (l4prog-l4entry (first res-args))
                           (l4prog-l4entry (second res-args)))]
               ['<= (l4biop (leeq)
                            (l4prog-l4entry (first res-args))
                            (l4prog-l4entry (second res-args)))]
               ['= (l4biop (eqal)
                           (l4prog-l4entry (first res-args))
                           (l4prog-l4entry (second res-args)))]
               ['number? (l4pred (numpred)
                                 (l4prog-l4entry (first res-args)))]
               ['a? (l4pred (arrpred)
                            (l4prog-l4entry (first res-args)))]
               ['read (l4read)]
               ['print (l4print (l4prog-l4entry (first res-args)))]
               ['new-array (l4newarr (l4prog-l4entry (first res-args))
                                     (l4prog-l4entry (second res-args)))]
               ['aref (l4arrref (l4prog-l4entry (first res-args))
                                (l4prog-l4entry (second res-args)))]
               ['aset (l4arrset (l4prog-l4entry (first res-args))
                                (l4prog-l4entry (second res-args))
                                (l4prog-l4entry (third res-args)))]
               ['alen (l4arrlen (l4prog-l4entry (first res-args)))]
               [else (error 'l5-compiler-call-prim "Unknow prim type ~a" prim)])
             (append funcs
                     (foldl (λ (a f) (append (l4prog-l4funcl a) f)) empty res-args)))]
    [else (error 'l5-compiler-call-prim "Prim type expected!")]))

(define/contract (l5-compiler-call-clos func args funcs)
  (-> L5e? (listof L5e?) (listof l4func?) l4prog?)
  (define func-clo (l4var (var-suffix 'l5_func_ 'L5)))
  (define res-func (l5-compiler func funcs))
  (define res-args (map (λ (a) (l5-compiler a empty)) args))
  (l4prog
;   (type-case L4Expression (l4prog-l4entry res-func)
;     [l4let (var value body)
;            (l4let var value
;                   (l4let func-clo body
;                          (l4funcal (l4clproc (l4value func-clo))
;                                    (cons (l4clvars (l4value func-clo))
;                                          (map l4prog-l4entry res-args)))))]
;     [else (l4let func-clo (l4prog-l4entry res-func)
;                  (l4funcal (l4clproc (l4value func-clo))
;                            (cons (l4clvars (l4value func-clo))
;                                  (map l4prog-l4entry res-args))))])
   (l4let func-clo (l4prog-l4entry res-func)
          (l4funcal (l4clproc (l4value func-clo))
                    (cons (l4clvars (l4value func-clo))
                          (map l4prog-l4entry res-args))))
   (append (l4prog-l4funcl res-func)
           (foldl (λ (a f) (append (l4prog-l4funcl a) f)) empty res-args))))                     
  
(define/contract (l5-compiler-let var value body funcs)
  (-> l5var? L5e? L5e? (listof l4func?) l4prog?)
  (define res-value (l5-compiler value funcs))
  (define res-body (l5-compiler body empty))
  (l4prog
   (l4let (l4var (unwrap-l5var var))
          (l4prog-l4entry res-value)
          (l4prog-l4entry res-body))
   (append (l4prog-l4funcl res-value) (l4prog-l4funcl res-body))))

(define/contract (l5-compiler-letrec var value body funcs)
  (-> l5var? L5e? L5e? (listof l4func?) l4prog?)
  (l5-compiler
   (l5let var
          (l5newtup (list (l5num 0)))
          (l5begin
           (l5call (l5prim 'aset)
                   (list var
                         (l5num 0)
                         (l5replace var
                                    (l5call (l5prim 'aref) (list var (l5num 0)))
                                    value)))
           (l5replace var
                      (l5call (l5prim 'aref) (list var (l5num 0)))
                      body)))
   funcs))

(define/contract (l5replace search replace exp)
  (-> l5var? L5e? L5e? L5e?)
  (type-case L5e exp
    [l5lambda (vars body)
              (l5lambda vars (l5replace search replace body))]
    [l5let (var value body)
           (l5let var
                  (l5replace search replace value)
                  (l5replace search replace body))]
    [l5letrec (var value body)
              (l5letrec var
                        (l5replace search replace value)
                        (l5replace search replace body))]
    [l5if (cond then else)
          (l5if (l5replace search replace cond)
                (l5replace search replace then)
                (l5replace search replace else))]
    [l5newtup (vals)
              (l5newtup (map (λ (v) (l5replace search replace v)) vals))]
    [l5begin (exp1 exp2)
             (l5begin (l5replace search replace exp1)
                      (l5replace search replace exp2))]
    [l5call (func args)
            (l5call (l5replace search replace func)
                    (map (λ (v) (l5replace search replace v)) args))]
    [l5num (num) exp]
    [l5prim (prim) exp]
    [l5var (var)
           (if (equal? var (unwrap-l5var search))
               replace
               exp)]))
    

(define/contract (l5-compiler-if cond then else funcs)
  (-> L5e? L5e? L5e? (listof l4func?) l4prog?)
  (define res-cond (l5-compiler cond funcs))
  (define res-then (l5-compiler then (l4prog-l4funcl res-cond)))
  (define res-else (l5-compiler else (l4prog-l4funcl res-then)))
  (l4prog
   (l4if (l4prog-l4entry res-cond)
         (l4prog-l4entry res-then)
         (l4prog-l4entry res-else))
   (l4prog-l4funcl res-else)))
  
(define/contract (l5-compiler-newtup vals funcs)
  (-> (listof L5e?) (listof l4func?) l4prog?)
  (define res-vals (map (λ (v) (l5-compiler v empty)) vals))
  (l4prog
   (l4newtup (map l4prog-l4entry res-vals))
   (append funcs (foldl append empty (map l4prog-l4funcl res-vals)))))

(define/contract (l5-compiler-begin exp1 exp2 funcs)
  (-> L5e? L5e? (listof l4func?) l4prog?)
  (define res-exp1 (l5-compiler exp1 funcs))
  (define res-exp2 (l5-compiler exp2 (l4prog-l4funcl res-exp1)))
  (l4prog
   (l4begin (l4prog-l4entry res-exp1)
            (l4prog-l4entry res-exp2))
   (l4prog-l4funcl res-exp2)))

(define/contract (l5-compiler-prim prim funcs)
  (-> symbol? (listof l4func?) l4prog?)
  (match prim
    [(or (? is-l5biop) 'new-array 'aref)
     (l5-compiler
      (l5lambda (list (l5var 'x) (l5var 'y))
                (l5call (l5prim prim) (list (l5var 'x) (l5var 'y))))
      funcs)]
    [(or (? is-l5pred) 'read 'print 'alen)
     (l5-compiler
      (l5lambda (list (l5var 'x)) (l5call (l5prim prim) (list (l5var 'x))))
      empty)]
    ['aset
     (l5-compiler
      (l5lambda (list (l5var 'x) (l5var 'y) (l5var 'z))
                (l5call (l5prim prim) (list (l5var 'x) (l5var 'y) (l5var 'z))))
      empty)]))
    
(define/contract (find-free-vars bound-vars free-vars exp)
  (-> (set/c symbol?) (set/c symbol?) L5e? (set/c symbol?))
  (type-case L5e exp
    [l5lambda (vars body)
              (let ([vars (map unwrap-l5var vars)])
                (let ([bound-vars (set-union bound-vars (list->set vars))])
                  (find-free-vars bound-vars free-vars body)))]
;              free-vars]
    [l5let (var value body)
           (find-free-vars (set-add bound-vars (unwrap-l5var var))
                           (find-free-vars bound-vars free-vars value)
                           body)]
    [l5letrec (var value body)
              (let ([bound-vars (set-add bound-vars (unwrap-l5var var))])
                (find-free-vars bound-vars
                                (find-free-vars bound-vars free-vars value)
                                body))]
    [l5if (cond then else)
          (let ([cond-free-vars
                 (find-free-vars bound-vars free-vars cond)])
            (let ([then-free-vars
                   (find-free-vars bound-vars cond-free-vars then)])
              (find-free-vars bound-vars then-free-vars else)))]
    [l5newtup (vals)
              (foldl (λ (e f) (find-free-vars bound-vars f e))
                     free-vars
                     vals)]
    [l5begin (exp1 exp2)
             (find-free-vars bound-vars
                             (find-free-vars bound-vars free-vars exp1)
                             exp2)]
    [l5call (func args)
            (let ([free-vars (find-free-vars bound-vars free-vars func)])
              (foldl (λ (e f) (find-free-vars bound-vars f e))
                     free-vars
                     args))]
    [l5num (num) free-vars]
    [l5prim (prim) free-vars]
    [l5var (var) (if (set-member? bound-vars var)
                     free-vars
                     (set-add free-vars var))]))