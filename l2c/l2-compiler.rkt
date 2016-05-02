#lang plai

(require "l2-definition.rkt")
(require "spill.rkt")
(require "graph.rkt")

(define/contract (l2-compiler p)
  (-> prog? (or/c prog? string?))
  (define l1-result (map l2-func-compiler (prog-funl p)))
  (if (equal? (ormap boolean? l1-result) #t)
      "could not register allocate"
      (prog (prog-name p) l1-result)))
  
(define/contract (l2-func-compiler f)
  (-> func? (or/c func? boolean?))
  (define b (box f))
  (define h (register-allocate b))
  (cond
    [(equal? #f h) #f]
    [(hash? h) (generate-l1 h (unbox b))]
    [else (error 'l2tol1 "unknown error")]))

(define/contract (register-allocate b)
  (-> box? (or/c hash? boolean?))
  (define g (build-graph (func-insl (unbox b))))
  (define res (coloring g))
  (cond
    [(equal? #f res) #f]
    [(hash? res) res]
    [(symbol? res) (begin
                     ;(printf "spiling ~a\n" res)
                     (set-box! b (spill-func (unbox b) res '__spill__))
                     ;(print-func (unbox b))
                     (register-allocate b))]
    [else (error 'register-allocate "unknown error ~a" res)]))

(define/contract (generate-l1 h f)
  (-> hash? func? func?)
  (func (func-name f) (func-narg f) (func-nspl f)
        (map (λ (i) (generate-l1-ins h i (func-nspl f))) (func-insl f))))

(define/contract (generate-l1-ins h i nspl)
  (-> hash? Inst? number? Inst?)
  (type-case Inst i
    [varia (vari) (regst (hash-ref h vari))]
    [loadi (sorc offs) (loadi (generate-l1-ins h sorc nspl) offs)]
    [stack (stak) (loadi (regst 'rsp) (+ (* 8 nspl) stak))]
    [movei (dest sorc) (movei (generate-l1-ins h dest nspl) (generate-l1-ins h sorc nspl))]
    [aropi (oper dest sorc) (aropi oper (generate-l1-ins h dest nspl) (generate-l1-ins h sorc nspl))]
    [sfopi (oper dest sorc) (sfopi oper (generate-l1-ins h dest nspl) (generate-l1-ins h sorc nspl))]
    [compi (comp dest lhs rhs)
           (compi comp (generate-l1-ins h dest nspl) (generate-l1-ins h lhs nspl) (generate-l1-ins h rhs nspl))]
    [cjmpi (comp lhs rhs true fals)
           (cjmpi comp (generate-l1-ins h lhs nspl) (generate-l1-ins h rhs nspl) true fals)]
    [calli (dest narg) (calli (generate-l1-ins h dest nspl) narg)]
    [tcall (dest narg) (tcall (generate-l1-ins h dest nspl) narg)]
    [else i]))

