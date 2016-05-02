#lang plai

(require "l2-definition.rkt")
(require "l2-parser.rkt")
(require "liveness.rkt")

;========== GRAPH COLORING ==========
(require graph)

(define/contract (generate-edges l)
  (-> (listof symbol?) (listof list?))
  (cond
    [(empty? l) empty]
    [else (generate-edges-helper (first l) (rest l))]))

(define/contract (generate-edges-helper r l)
  (-> symbol? (listof symbol?) (listof list?))
  (cond
    [(empty? l) empty]
    [else (append (map (λ (e) (list r e)) l)
                  (generate-edges-helper (first l) (rest l)))]))

(define/contract (movei-patch ins)
  (-> Inst? set?)
  (type-case Inst ins
    [movei (dest sorc)
           (type-case Inst dest
             [varia (vard)
                    (type-case Inst sorc
                      [varia (vars) (set (list vard vars) (list vars vard))]
                      [regst (regs) (set (list vard regs) (list regs vard))]
                      [else (set)])]
             [regst (regd)
                    (type-case Inst sorc
                      [varia (vars) (set (list regd vars) (list vars regd))]
                      [regst (regs) (set (list regd regs) (list regs regd))]
                      [else (set)])]
             [else (set)])]
    [else (set)]))

(define/contract (sfopi-patch ins)
  (-> Inst? set?)
  (type-case Inst ins
    [sfopi (oper dest sorc)
           (type-case Inst sorc
             [varia (vars)
                    (set (list vars 'r10) (list vars 'r11) (list vars 'r12) (list vars 'r13) (list vars 'r14)
                         (list vars 'r15) (list vars 'r8)  (list vars 'r9)  (list vars 'rax) (list vars 'rbp)
                         (list vars 'rbx) (list vars 'rdi) (list vars 'rdx) (list vars 'rsi))]
             [else (set)])]
    [else (set)]))

(define/contract (interference ins in out 1st)
  (-> Inst? set-mutable? set-mutable? boolean? set?)
  (set-union
   ;appearing together in the first instructions's 'in' set
   (if 1st
       (list->set (generate-edges (set->list in)))
       (set))
   (set-subtract
    (list->set (generate-edges (set->list
								;appearing together in an 'out' set
                                (set-union (list->set (set->list out))
								;killed variables interfere with variables in the out set
                                           (list->set (set->list (killgen-kill (kill&gen ins))))))))
    (movei-patch ins))
   (sfopi-patch ins)))

(define/contract (vertex-patch ins)
  (-> Inst? (listof symbol?))
  (define kg (kill&gen ins))
  (set->list (set-union (list->set (set->list (killgen-kill kg)))
                        (list->set (set->list (killgen-gen kg))))))

(define/contract (build-graph insl)
  (-> (listof Inst?) graph?)
  ; initialize the graph such that all registers interfere with each other
  (define g (undirected-graph
             (generate-edges
              '(r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi))))
  (define io (in&out insl))
  (for ([ins insl]
        [i (in-range 1 (+ 1 (length insl)))]
        [out (inout-out io)]
        [in (inout-in io)])
    (if (= i 1)
        (for ([e (interference ins in out #t)])
          (add-edge! g (first e) (second e)))
        (for ([e (interference ins in out #f)])
          (add-edge! g (first e) (second e))))
    (for ([v (vertex-patch ins)])
      (add-vertex! g v)))
  g)

(define/contract (find-vertex g)
  (-> graph? (or/c symbol? boolean?))
  (define vertex #f)
  (define max -1)
  (for ([v (in-vertices g)])
    (let ([n (length (get-neighbors g v))])
      (when (and (< n 15) (> n max))
        (set! vertex v))))
  vertex)

(define/contract (coloring g)
  (-> graph? (or/c (listof pair?) boolean?))
  (define g2 (graph-copy g))
  (define v #t)
  (define stack empty)
  (define res (make-hash))
  ;remove
  (for ([i (in-range (length (get-vertices g)))])
    (set! v (find-vertex g))
    #:break (equal? v #f)
;    (printf "pick vertex ~a\n" v)
    (remove-vertex! g v)
    (set! stack (cons v stack)))
  ;add
  (if (equal? v #f)
      #f
      (begin
        (for-each (λ (v)
                    (when (not (isregister? v))
                      (let ([reg (mutable-set 'r10 'r11 'r12 'r13 'r14 'r15 'r8'r9
                                              'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)]
                            [nei (list->mutable-set (get-neighbors g2 v))])
                        (set-subtract! reg nei)
                        (for ([n nei])
                          (when (and (not (isregister? n)) (not (equal? (hash-ref res n #f) #f)))
                            (set-remove! reg (hash-ref res n))))
                        (hash-set! res v (assign-reg reg)))))
                  stack)   
        (sort (hash->list res) (λ (a b) (symbol<? (car a) (car b)))))))

(define/contract (assign-reg regs)
  (-> set-mutable? symbol?)
  (cond
    [(set-member? regs 'r10) 'r10]
    [(set-member? regs 'r11) 'r11]
    [(set-member? regs 'r12) 'r12]
    [(set-member? regs 'r13) 'r13]
    [(set-member? regs 'r14) 'r14]
    [(set-member? regs 'r15) 'r15]
    [(set-member? regs 'r8) 'r8]
    [(set-member? regs 'r9) 'r9]
    [(set-member? regs 'rax) 'rax]
    [(set-member? regs 'rbp) 'rbp]
    [(set-member? regs 'rbx) 'rbx]
    [(set-member? regs 'rcx) 'rcx]
    [(set-member? regs 'rdi) 'rdi]
    [(set-member? regs 'rdx) 'rdx]
    [(set-member? regs 'rsi) 'rsi]
    [else (error 'assign-reg "can not assign a register")]))