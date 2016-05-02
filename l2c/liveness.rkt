#lang plai

(require "l2-definition.rkt")

;========== LIVENESS ==========
(define/contract (kill&gen ins)
  (-> Inst? killgen?)
  (define res (killgen (mutable-set) (mutable-set)))
  (type-case Inst ins
    [movei (dest sorc)
           (begin
             (type-case Inst dest
               [varia (vard) (set-add! (killgen-kill res) vard)]
               [regst (regd) (set-add! (killgen-kill res) regd)]
               [loadi (srcl offl)
                      (type-case Inst srcl
                        [varia (varl) (set-add! (killgen-gen res) varl)]
                        [regst (regl)
                               (when (not (eq? regl 'rsp))
                                 (set-add! (killgen-gen res) regl))]
                        [else (error 'kill&gen "Invalid instruction ~a" ins)])]
               [else (error 'kill&gen "Invalid instruction ~a" ins)])
             (type-case Inst sorc
               [varia (vars) (set-add! (killgen-gen res) vars)]
               [regst (regs)
                      (when (not (eq? regs 'rsp))
                        (set-add! (killgen-gen res) regs))]
               [loadi (srcl offl)
                      (type-case Inst srcl
                        [regst (regl)
                               (when (not (eq? regl 'rsp))
                                 (set-add! (killgen-gen res) regl))]
                        [varia (varl) (set-add! (killgen-gen res) varl)]
                        [else (error 'kill&gen "Invalid instruction ~a" ins)])]
               [numbr (num)  (void)]
               [label (lab)  (void)]
               [stack (stak) (void)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)]))]
    [aropi (oper dest sorc)
           (begin
             (type-case Inst dest
               [varia (vard)
                      (begin
                       (set-add! (killgen-kill res) vard)
                       (set-add! (killgen-gen  res) vard))]
               [regst (regd)
                      (begin
                        (set-add! (killgen-kill res) regd)
                        (set-add! (killgen-gen  res) regd))]
               [else (error 'kill&gen "Invalid instruction ~a" ins)])
             (type-case Inst sorc
               [varia (vars) (set-add! (killgen-gen res) vars)]
               [regst (regs) (set-add! (killgen-gen res) regs)]
               [numbr (num)  (void)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)]))]
    [sfopi (oper dest sorc)
           (begin
             (type-case Inst dest
               [varia (vard)
                      (begin
                       (set-add! (killgen-kill res) vard)
                       (set-add! (killgen-gen  res) vard))]
               [regst (regd)
                      (begin
                        (set-add! (killgen-kill res) regd)
                        (set-add! (killgen-gen  res) regd))]
               [else (error 'kill&gen "Invalid instruction ~a" ins)])
             (type-case Inst sorc
               [varia (vars) (set-add! (killgen-gen res) vars)]
               [regst (regs) (set-add! (killgen-gen res) regs)]
               [numbr (num)  (void)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)]))]
    [compi (comp dest lhs rhs)
           (begin
             (type-case Inst dest
               [varia (vard) (set-add! (killgen-kill res) vard)]
               [regst (regd) (set-add! (killgen-kill res) regd)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)])
             (type-case Inst lhs
               [varia (varl) (set-add! (killgen-gen  res) varl)]
               [regst (regl) (set-add! (killgen-gen  res) regl)]
               [numbr (num)  (void)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)])
             (type-case Inst rhs
               [varia (varr) (set-add! (killgen-gen  res) varr)]
               [regst (regr) (set-add! (killgen-gen  res) regr)]
               [numbr (num)  (void)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)]))]
    [cjmpi (comp lhs rhs true fals)
           (begin
             (type-case Inst lhs
               [varia (varl) (set-add! (killgen-gen  res) varl)]
               [regst (regl) (set-add! (killgen-gen  res) regl)]
               [numbr (num)  (void)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)])
             (type-case Inst rhs
               [varia (varr) (set-add! (killgen-gen  res) varr)]
               [regst (regr) (set-add! (killgen-gen  res) regr)]
               [numbr (num)  (void)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)]))]
    [calli (dest narg)
           (begin
             (type-case Inst dest
               [varia (vard) (set-add! (killgen-gen res) vard)]
               [regst (regd) (set-add! (killgen-gen res) regd)]
               [label (labl) (void)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)])
             (gen-args res narg)
             (kill-caller-save-regs res))]
    [tcall (dest narg)
           (begin
             (type-case Inst dest
               [varia (vard) (set-add! (killgen-gen res) vard)]
               [regst (regd) (set-add! (killgen-gen res) regd)]
               [label (labl) (void)]
               [else (error 'kill&gen "Invalid instruction ~a" ins)])
             (gen-args res narg)
             (gen-callee-save-regs res))]
    [retun ()
            (begin
              (set-add! (killgen-gen res) 'rax)
              (gen-callee-save-regs res))]
    [cprit ()
           (begin
             (gen-args res 1)
             (kill-caller-save-regs res))]
    [caloc ()
           (begin
             (gen-args res 2)
             (kill-caller-save-regs res))]
    [caerr ()
           (begin
             (gen-args res 2)
             (kill-caller-save-regs res))]
    [else (void)])
  res)

(define/contract (gen-args res narg)
  (-> killgen? number? void?)
  (when (>= narg 1) (set-add! (killgen-gen res) 'rdi))
  (when (>= narg 2) (set-add! (killgen-gen res) 'rsi))
  (when (>= narg 3) (set-add! (killgen-gen res) 'rdx))
  (when (>= narg 4) (set-add! (killgen-gen res) 'rcx))
  (when (>= narg 5) (set-add! (killgen-gen res) 'r8))
  (when (>= narg 6) (set-add! (killgen-gen res) 'r9)))

(define/contract (kill-caller-save-regs res)
  (-> killgen? void?)
  (set-add! (killgen-kill res) 'r10)
  (set-add! (killgen-kill res) 'r11)
  (set-add! (killgen-kill res) 'r8)
  (set-add! (killgen-kill res) 'r9)
  (set-add! (killgen-kill res) 'rax)
  (set-add! (killgen-kill res) 'rcx)
  (set-add! (killgen-kill res) 'rdi)
  (set-add! (killgen-kill res) 'rdx)
  (set-add! (killgen-kill res) 'rsi))

(define/contract (gen-callee-save-regs res)
  (-> killgen? void?)
  (set-add! (killgen-gen res) 'r12)
  (set-add! (killgen-gen res) 'r13)
  (set-add! (killgen-gen res) 'r14)
  (set-add! (killgen-gen res) 'r15)
  (set-add! (killgen-gen res) 'rbp)
  (set-add! (killgen-gen res) 'rbx))

(define/contract (find-predecessor insl)
  (-> (listof Inst?) vector?)
  (define res (make-vector (length insl) empty))
  (for ([i (in-range (length insl))]
        [ins insl])
    (type-case Inst ins
      [gotoi (labl)
             (let ([j (find-label insl labl)])
               (vector-set! res j (cons i (vector-ref res j))))]
      [cjmpi (comp lhs rhs true fals)
             (let ([j (find-label insl true)]
                   [k (find-label insl fals)])
               (vector-set! res j (cons i (vector-ref res j)))
               (vector-set! res k (cons i (vector-ref res k))))]
      [retun () (void)]
      [tcall (dest narg) (void)]
      [caerr () (void)]
      [else (when (< (+ i 1) (vector-length res))
              (vector-set! res (+ i 1) (cons i (vector-ref res (+ i 1)))))]))
  res)

(define/contract (find-label insl target)
  (-> (listof Inst?) Inst? number?)
  (cond
    [(cons? insl)
     (if (equal? (first insl) target)
         0
         (+ 1 (find-label (rest insl) target)))]
    [else
     (error 'find-label "can not find label ~a" target)]))

(define/contract (in&out insl)
  (-> (listof Inst?) inout?)
  (define res (inout (make-vector (length insl))
                     (make-vector (length insl))))
  (for ([i (in-range (vector-length (inout-in res)))])
    (vector-set! (inout-in res) i (mutable-set))
    (vector-set! (inout-out res) i (mutable-set)))
  (define kg-vec (list->vector (map kill&gen insl)))
  (define pre-vec (find-predecessor insl))
  (for ([i (in-range (vector-length (inout-in res)))])
    (calc-inout (inout-in res) (inout-out res) kg-vec pre-vec))
  res)

(define/contract (calc-inout in-vec out-vec kg-vec pre-vec)
  (-> vector? vector? vector? vector? void?)
  (vector-map calc-in in-vec out-vec kg-vec)
  (for ([i (in-range (vector-length in-vec))])
    (for ([j (vector-ref pre-vec i)])
      (set-union! (vector-ref out-vec j) (vector-ref in-vec i))))
  (void))

(define/contract (calc-in in-set out-set kg)
  (-> set-mutable? set-mutable? killgen? void?)
  (define temp (set-copy out-set))
  (set-subtract! temp (killgen-kill kg))
  (set-union! in-set (killgen-gen kg) temp))