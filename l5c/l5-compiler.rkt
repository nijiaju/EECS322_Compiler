#lang plai

(require "l5-definition.rkt" "../l4c/l4-definition.rkt" "../l3c/l3-parser.rkt")
(require "l5-parser.rkt")

(define/contract (l5-compiler e funcs)
  (-> L5e? (listof l4func?) l4prog?)
  (type-case L5e e
    [l5lambda (vars body) (l5-compiler-lambda vars body funcs)]
    [l5var (var) (l4prog (l4value (l4var var)) funcs)]
    [else (error 'l5-compiler "Not Implemented: ~a" e)]))

(define/contract (l5-compiler-lambda vars body funcs)
  (-> (listof l5var?) L5e? (listof l4func?) l4prog?)
  (define free-vars (find-free-vars (list->set (map unwrap-l5var vars)) (set) body))
  (define func-vars (map l4value (map l4var (set->list free-vars))))
  (define func-name (label-suffix ':l5-lambda- 'L5))
  (define cmpd-body (l5-compiler body empty))
  (l4prog (l4makecl func-name (l4newtup func-vars))
          (cons (l4func func-name func-vars
                        (l4prog-l4entry cmpd-body))
                (append funcs (l4prog-l4funcl cmpd-body)))))
                        
  
(define/contract (find-free-vars bound-vars free-vars exp)
  (-> (set/c symbol?) (set/c symbol?) L5e? (set/c symbol?))
  (type-case L5e exp
    [l5lambda (vars body)
;              (let ([vars (map unwrap-l5var vars)])
;                (let ([bound-vars (set-union bound-vars (list->set vars))])
;                  (find-free-vars bound-vars free-vars body)))
              free-vars]
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