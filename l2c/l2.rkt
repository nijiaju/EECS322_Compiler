#lang plai

;========== TYPE DEFINITION ==========
(struct prog (name funl))

(struct func (name narg nspl insl))

(define-type Inst
  [numbr (numb number?)]
  [regst (regs symbol?)]
  [label (labl symbol?)]
  [varia (vari symbol?)]
  [loadi (sorc Inst?) (offs number?)]
  [stack (stak number?)]
  [movei (dest Inst?) (sorc Inst?)]
  [aropi (oper Aop?) (dest Inst?) (sorc Inst?)]
  [sfopi (oper Sop?) (dest Inst?) (sorc Inst?)]
  [compi (comp Cmp?) (dest Inst?) (lhs  Inst?) (rhs  Inst?)]
  [cjmpi (comp Cmp?) (lhs  Inst?) (rhs  Inst?) (true Inst?) (fals Inst?)]
  [gotoi (labl Inst?)]
  [calli (dest Inst?) (narg number?)]
  [tcall (dest Inst?) (narg number?)]
  [cprit]
  [caloc]
  [caerr]
  [retun])

(define-type Aop
  [addop]
  [subop]
  [mulop]
  [andop])

(define-type Sop
  [sflft]
  [sfrht])

(define-type Cmp
  [less]
  [leeq]
  [eqal])

(struct killgen (kill gen))

(struct inout (in out))

;========== PARSER ==========

(define (parsep sexp)
  (match sexp
    [`(,e1 ,e2 ...) (prog e1 (map parsef e2))]
    [_ (error 'parsep "syntax error")]))

(define (parsef sexp)
  (match sexp
    [`(,e1 ,e2 ,e3 ,e4 ...) (func e1 e2 e3 (map parsei e4))]
    [_ (error 'parsef "syntax error")]))

(define (parsei sexp)
  (match sexp
    [`(,e1 <- ,e2)               (movei (parsei e1) (parsei e2))]
    [`(mem ,e1 ,e2)              (loadi (parsei e1) e2)]
    [`(stack-arg ,e1)            (stack e1)]
    [`(,e1 += ,e2)               (aropi (addop) (parsei e1) (parsei e2))]
    [`(,e1 -= ,e2)               (aropi (subop) (parsei e1) (parsei e2))]
    [`(,e1 *= ,e2)               (aropi (mulop) (parsei e1) (parsei e2))]
    [`(,e1 &= ,e2)               (aropi (andop) (parsei e1) (parsei e2))]
    [`(,e1 <<= ,e2)              (sfopi (sflft) (parsei e1) (parsei e2))]
    [`(,e1 >>= ,e2)              (sfopi (sfrht) (parsei e1) (parsei e2))]
    [`(,e1 <- ,e2 < ,e3)         (compi (less)  (parsei e1) (parsei e2) (parsei e3))]
    [`(,e1 <- ,e2 <= ,e3)        (compi (leeq)  (parsei e1) (parsei e2) (parsei e3))]
    [`(,e1 <- ,e2 = ,e3)         (compi (eqal)  (parsei e1) (parsei e2) (parsei e3))]
    [`(cjump ,e1 < ,e2 ,e3, e4)  (cjmpi (less) (parsei e1) (parsei e2) (parsei e3) (parsei e4))]
    [`(cjump ,e1 <= ,e2 ,e3, e4) (cjmpi (leeq) (parsei e1) (parsei e2) (parsei e3) (parsei e4))]
    [`(cjump ,e1 = ,e2 ,e3, e4)  (cjmpi (eqal) (parsei e1) (parsei e2) (parsei e3) (parsei e4))]
    [`(goto ,e1)                 (gotoi (parsei e1))]
    [`(call print 1)             (cprit)]
    [`(call allocate 2)          (caloc)]
    [`(call array-error 2)       (caerr)]
    [`(call ,e1 ,e2)             (calli (parsei e1) e2)]
    [`(tail-call ,e1 ,e2)        (tcall (parsei e1) e2)]
    [`(return)                   (retun)]
    [(? number?)                 (numbr sexp)]
    [(? is-label?)                (label sexp)]
    [(? is-register?)             (regst sexp)]
    [(? is-variable?)             (varia sexp)]
    [else                        (error 'parsei "syntax error")]))

(define (is-label? s)
  (match (symbol->string s)
    [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*") #t]
    [_ #f]))

(define/contract (is-variable? s)
  (-> symbol? boolean?)
  (match (symbol->string s)
    [(regexp #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$") #t]
    [_ #f]))

(define/contract (is-register? s)
  (-> symbol? boolean?)
  (match (symbol->string s)
    [(regexp #rx"^(rdi|rsi|rdx|rcx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp)$") #t]
    [_ #f]))

;========== SPILL ==========
(define/contract (is-spilled-register? s)
  (-> symbol? boolean?)
  (string-prefix? (symbol->string s) "__spill__"))

(define/contract (new-var p n)
  (-> symbol? number? Inst?)
  (varia (string->symbol
          (string-append
           (symbol->string p)
           (number->string n)))))

(define/contract (spill-assign ins-list var n ins-type)
  (-> (listof Inst?) Inst? number? symbol? (listof Inst?))
  (cond
    [(cons? (rest ins-list))
     (cons (first ins-list) (spill-assign (rest ins-list) var n ins-type))]
    [(empty? (rest ins-list))
     (cond
       [(eq? ins-type 'memsorc+spldest)
        (type-case Inst (first ins-list)
          [loadi (lsrc loff)
                 (list (movei var (first ins-list))
                       (movei (loadi (regst 'rsp) (* n 8)) var))]
          [else (error 'spill "syntax error ~a" (first ins-list))])]
       [(eq? ins-type 'memsorc)
        (type-case Inst (first ins-list)
          [loadi (lsrc loff)
                 (list (movei var (first ins-list)))]
          [else (error 'spill "syntax error ~a" (first ins-list))])]
       [(eq? ins-type 'splmemdest)
        (type-case Inst (first ins-list)
          [loadi (lsrc loff)
                 (list (movei (first ins-list) var))]
          [else (error 'spill "syntax error ~a" (first ins-list))])]
       [(eq? ins-type 'memdest+splsorc)
        (type-case Inst (first ins-list)
          [loadi (lsrc loff)
                 (list (movei var (loadi (regst 'rsp) (* n 8)))
                       (movei (first ins-list) var))]
          [else (error 'spill "syntax error ~a" (first ins-list))])])]))

(define/contract (spill-func f v p)
  (-> func? symbol? symbol? func?)
  (let ([counter (box 0)]
        [name (func-name f)]
        [narg (func-narg f)]
        [nspl (func-nspl f)]
        [insl (func-insl f)])
    (func name narg (+ nspl 1)
          (foldl
           (λ (i res) (append res (spill i nspl counter v p)))
           empty
           insl))))

(define/contract (spill i n c v p)
  (-> Inst? number? box? symbol? symbol? (listof Inst?))
  (type-case Inst i
    [numbr (numb) (list i)]
    [regst (regs) (list i)]
    [label (labl) (list i)]
    [varia (vari)
           (cond
             [(eq? vari v) (list (new-var p (unbox c)))]
             [else (list i)])]
    [loadi (sorc offs)
           (type-case Inst sorc
             [varia (vars)
                    (cond
                      [(eq? vars v)
                         (list (movei (new-var p (unbox c)) (loadi (regst 'rsp) (* n 8)))
                               (loadi (new-var p (unbox c)) offs))]
                      [else (list i)])]
             [else (list i)])]
    [movei (dest sorc)
           (type-case Inst dest
             [varia (vard)
                    (cond
                      [(eq? vard v)
                       (type-case Inst sorc
                         [varia (vars)
                                (cond
                                  [(eq? vars v) empty]
                                  [else (list
                                         (movei (loadi (regst 'rsp) (* n 8)) sorc))])]
                         [loadi (lsrc loff)
                                (begin0
                                  (spill-assign (spill sorc n c v p) (new-var p (unbox c))
                                                n 'memsorc+spldest)
                                  (set-box! c (+ 1 (unbox c))))]
                         [stack (offs)
                                (begin0
                                  (list (movei (new-var p (unbox c)) sorc)
                                        (movei (loadi (regst 'rsp) (* n 8)) (new-var p (unbox c))))
                                  (set-box! c (+ 1 (unbox c))))]
                         [else (list (movei (loadi (regst 'rsp) (* n 8)) sorc))])]
                      [else
                       (type-case Inst sorc
                         [varia (vars)
                                (cond
                                  [(eq? vars v) (list
                                                 (movei dest (loadi (regst 'rsp) (* n 8))))]
                                  [else (list i)])]
                         [loadi (lsrc loff)
                                (begin0
                                  (spill-assign (spill sorc n c v p) dest n 'memsorc)
                                  (when (and (varia? lsrc) (equal? lsrc (varia v)))
                                    (set-box! c (+ 1 (unbox c)))))]
                         [else (list i)])])]
             [loadi (lsrc loff)
                    (type-case Inst lsrc
                      [varia (vars)
                             (cond
                               [(eq? vars v)
                                (begin0
                                  (spill-assign (spill dest n c v p) (first (spill sorc n c v p))
                                                n 'splmemdest)
                                  (set-box! c (+ 1 (unbox c))))]
                               [else
                                (if (and (varia? sorc) (equal? sorc (varia v)))
                                    (begin0
                                      (spill-assign (spill dest n c v p) (first (spill sorc n c v p))
                                                    n 'memdest+splsorc)
                                      (set-box! c (+ 1 (unbox c))))
                                    (list i))])]
                      [else
                       (if (and (varia? sorc) (equal? sorc (varia v)))
                           (begin0
                             (spill-assign (spill dest n c v p) (first (spill sorc n c v p))
                                           n 'memdest+splsorc)
                             (set-box! c (+ 1 (unbox c))))
                           (list i))])]
             [regst (regs)
                    (type-case Inst sorc
                      [varia (vars)
                             (cond
                               [(eq? vars v) (list
                                              (movei dest (loadi (regst 'rsp) (* n 8))))]
                               [else (list i)])]
                      [loadi (lsrc loff)
                             (begin0
                               (spill-assign (spill sorc n c v p) dest n 'memsorc)
                               (when (and (varia? lsrc) (equal? lsrc (varia v)))
                                 (set-box! c (+ 1 (unbox c)))))]
                      [else (list i)])]
             [else (error 'spill "syntax error ~a" i)])]
    [aropi (oper dest sorc)
           (type-case Inst dest
             [varia (vard)
                    (cond
                      [(eq? vard v)
                       (begin0
                         (list (movei (first (spill dest n c v p)) (loadi (regst 'rsp) (* n 8)))
                               (aropi oper (first (spill dest n c v p)) (first (spill sorc n c v p)))
                               (movei (loadi (regst 'rsp) (* n 8)) (first (spill dest n c v p))))
                         (set-box! c (+ 1 (unbox c))))]
                      [else
                       (type-case Inst sorc
                         [varia (vars)
                                (cond
                                  [(eq? vars v)
                                   (begin0
                                     (list (movei (first (spill sorc n c v p)) (loadi (regst 'rsp) (* n 8)))
                                           (aropi oper (first (spill dest n c v p)) (first (spill sorc n c v p))))
                                     (set-box! c (+ 1 (unbox c))))]
                                  [else (list i)])]
                         [else (list i)])])]
             [else
              (type-case Inst sorc
                [varia (vars)
                        (cond
                          [(eq? vars v)
                           (begin0
                             (list (movei (first (spill sorc n c v p)) (loadi (regst 'rsp) (* n 8)))
                                   (aropi oper (first (spill dest n c v p)) (first (spill sorc n c v p))))
                             (set-box! c (+ 1 (unbox c))))]
                          [else (list i)])]
                [else (list i)])])]
    [sfopi (oper dest sorc)
           (type-case Inst dest
             [varia (vard)
                    (cond
                      [(eq? vard v)
                       (begin0
                         (list (movei (first (spill dest n c v p)) (loadi (regst 'rsp) (* n 8)))
                               (sfopi oper (first (spill dest n c v p)) (first (spill sorc n c v p)))
                               (movei (loadi (regst 'rsp) (* n 8)) (first (spill dest n c v p))))
                         (set-box! c (+ 1 (unbox c))))]
                      [else
                       (type-case Inst sorc
                         [varia (vars)
                                (cond
                                  [(eq? vars v)
                                   (begin0
                                     (list (movei (first (spill sorc n c v p)) (loadi (regst 'rsp) (* n 8)))
                                           (sfopi oper (first (spill dest n c v p)) (first (spill sorc n c v p))))
                                     (set-box! c (+ 1 (unbox c))))]
                                  [else (list i)])]
                         [else (list i)])])]
             [else
              (type-case Inst sorc
                [varia (vars)
                        (cond
                          [(eq? vars v)
                           (begin0
                             (list (movei (first (spill sorc n c v p)) (loadi (regst 'rsp) (* n 8)))
                                   (sfopi oper (first (spill dest n c v p)) (first (spill sorc n c v p))))
                             (set-box! c (+ 1 (unbox c))))]
                          [else (list i)])]
                [else (list i)])])]
    [compi (comp dest lhs rhs)
           (type-case Inst dest
             [varia (vard)
                    (cond
                      [(eq? vard v)
                       (begin0
                         (if (or (and (varia? lhs) (equal? lhs (varia v))) (and (varia? rhs) (equal? rhs (varia v))))
                             (list (movei (first (spill dest n c v p)) (loadi (regst 'rsp) (* n 8)))
                                   (compi comp (first (spill dest n c v p))
                                          (first (spill lhs n c v p)) (first (spill rhs n c v p)))
                                   (movei (loadi (regst 'rsp) (* n 8)) (first (spill dest n c v p))))
                             (list (compi comp (first (spill dest n c v p)) lhs rhs)
                                   (movei (loadi (regst 'rsp) (* n 8)) (first (spill dest n c v p)))))
                         (set-box! c (+ 1 (unbox c))))]
                      [else
                       (if (or (and (varia? lhs) (equal? lhs (varia v))) (and (varia? rhs) (equal? rhs (varia v))))
                           (begin0
                             (list (movei (new-var p (unbox c)) (loadi (regst 'rsp) (* n 8)))
                                   (compi comp dest (first (spill lhs n c v p)) (first (spill rhs n c v p))))
                             (set-box! c (+ 1 (unbox c))))
                           (list i))])]
             [else
              (if (or (and (varia? lhs) (equal? lhs (varia v))) (and (varia? rhs) (equal? rhs (varia v))))
                  (begin0
                    (list (movei (new-var p (unbox c)) (loadi (regst 'rsp) (* n 8)))
                          (compi comp dest (first (spill lhs n c v p)) (first (spill rhs n c v p))))
                    (set-box! c (+ 1 (unbox c))))
                  (list i))])]
    [cjmpi (comp lhs rhs true fals)
           (if (or (and (varia? lhs) (equal? lhs (varia v))) (and (varia? rhs) (equal? rhs (varia v))))
               (begin0
                 (list (movei (new-var p (unbox c)) (loadi (regst 'rsp) (* n 8)))
                       (cjmpi comp (first (spill lhs n c v p)) (first (spill rhs n c v p)) true fals))
                 (set-box! c (+ 1 (unbox c))))
               (list i))]
    [calli (dest narg)
           (if (and (varia? dest) (equal? dest (varia v)))
               (begin0
                 (list (movei (first (spill dest n c v p)) (loadi (regst 'rsp) (* n 8)))
                       (calli (first (spill dest n c v p)) narg))
                 (set-box! c (+ 1 (unbox c))))
               (list i))]
    [tcall (dest narg)
           (if (and (varia? dest) (equal? dest (varia v)))
               (begin0
                 (list (movei (first (spill dest n c v p)) (loadi (regst 'rsp) (* n 8)))
                       (tcall (first (spill dest n c v p)) narg))
                 (set-box! c (+ 1 (unbox c))))
               (list i))]
    [else (list i)]))

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
   (if 1st
       (list->set (generate-edges (set->list in)))
       (set))
   (set-subtract
    (list->set (generate-edges (set->list
                                (set-union (list->set (set->list out))
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

(define/contract (find-hint g)
  (-> graph? (or/c symbol? boolean?))
  (define vertex #f)
  (define max -1)
  (for ([v (in-vertices g)])
    (when (and (not (is-register? v)) (not (is-spilled-register? v)))
      (let ([n (length (get-neighbors g v))])
        (when (> n max)
          (set! vertex v)))))
  vertex)

(define/contract (coloring g)
  (-> graph? (or/c hash? symbol? boolean?))
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
      (find-hint g)
      (begin
        (for-each (λ (v)
                    (when (not (is-register? v))
                      (let ([reg (mutable-set 'r10 'r11 'r12 'r13 'r14 'r15 'r8'r9
                                              'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)]
                            [nei (list->mutable-set (get-neighbors g2 v))])
                        (set-subtract! reg nei)
                        (for ([n nei])
                          (when (and (not (is-register? n)) (not (equal? (hash-ref res n #f) #f)))
                            (set-remove! reg (hash-ref res n))))
                        (hash-set! res v (assign-reg reg)))))
                  stack)   
        res)))

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

;========== L2 COMPILER ==========
(define/contract (l2-compiler p)
  (-> prog? (or/c prog? boolean?))
  (prog (prog-name p)
        (map l2-func-compiler (prog-funl p))))
  
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
    [else (error 'register-allocate "unknown error")]))

(define/contract (generate-l1 h f)
  (-> hash? func? func?)
  (func (func-name f) (func-narg f) (func-nspl f)
        (map (λ (i) (generate-l1-ins h i (func-nspl f))) (func-insl f))))

(define/contract (generate-l1-ins h i nspl)
  (-> hash? Inst? number? Inst?)
  (type-case Inst i
    [varia (vari) (regst (hash-ref h vari))]
    [loadi (sorc offs) (loadi (generate-l1-ins h sorc nspl) offs)]
    [stack (stak) (loadi (regst 'rsp) (+ nspl stak))]
    [movei (dest sorc) (movei (generate-l1-ins h dest nspl) (generate-l1-ins h sorc nspl))]
    [aropi (oper dest sorc) (aropi oper (generate-l1-ins h dest nspl) (generate-l1-ins h sorc nspl))]
    [sfopi (oper dest sorc) (aropi oper (generate-l1-ins h dest nspl) (generate-l1-ins h sorc nspl))]
    [compi (comp dest lhs rhs)
           (compi (generate-l1-ins h dest nspl) (generate-l1-ins h lhs nspl) (generate-l1-ins h rhs nspl))]
    [cjmpi (comp lhs rhs true fals)
           (cjmpi (generate-l1-ins h lhs nspl) (generate-l1-ins h rhs nspl) true fals)]
    [calli (dest narg) (calli (generate-l1-ins h dest nspl) narg)]
    [tcall (dest narg) (tcall (generate-l1-ins h dest nspl) narg)]
    [else i]))

;========== INPUT/OUTPUT ==========
(define (display-inout res)
  (displayln "((in")
  (vector-map display-set (inout-in res))
  (displayln ")")
  (displayln "(out")
  (vector-map display-set (inout-out res))
  (displayln "))"))

(define (display-set s)
  (define l (sort (set->list s) symbol<?))
  (display "(")
  (for/list ([e l])
    (display e)
    (display " "))
  (displayln ")"))

(define (display-graph g)
  (define vs (sort (get-vertices g) symbol<?))
  (display "(")
  (for ([v vs])
    (displayln (cons v (sort (get-neighbors g v) symbol<?))))
  (display ")\n"))

(define (display-prog p)
  (printf "(~a\n~a)\n" (prog-name p)
          (foldr string-append "" (map format-func (prog-funl p)))))

(define (format-func f)
  (format "(~a ~a ~a\n~a)\n" (func-name f) (func-narg f) (func-nspl f)
          (foldr string-append "" (map format-ins (func-insl f)))))

(define (format-ins i)
  (type-case Inst i
    [numbr (numb) (format "~a" numb)]
    [regst (regs) (format "~a" regs)]
    [label (labl) (format "~a " labl)]
    [varia (vari) (format "~a" vari)]
    [loadi (sorc offs) (format " (mem ~a ~a) " (format-ins sorc) offs)]
    [stack (stak) (format " (stack-arg ~a) " stak)]
    [movei (dest sorc) (format "(~a <- ~a)\n" (format-ins dest) (format-ins sorc))]
    [aropi (oper dest sorc) (format "(~a ~a ~a)\n" (format-ins dest) (format-arop oper) (format-ins sorc))]
    [sfopi (oper dest sorc) (format "(~a ~a ~a)\n" (format-ins dest) (format-sfop oper) (format-ins sorc))]
    [compi (comp dest lhs rhs) (format "(~a <- ~a ~a ~a)\n"
                                       (format-ins dest) (format-ins lhs)
                                       (format-comp comp) (format-ins rhs))]
    [cjmpi (comp lhs rhs true fals) (format "(cjump ~a ~a ~a ~a ~a)\n"
                                            (format-ins lhs) (format-comp comp) (format-ins rhs)
                                            (format-ins true) (format-ins fals))]
    [gotoi (labl) (format "(goto ~a)\n" (format-ins labl))]
    [calli (dest narg) (format "(call ~a ~a)\n" (format-ins dest) narg)]
    [tcall (dest narg) (format "(tail-call ~a ~a)\n" (format-ins dest) narg)]
    [cprit () "(call print 1)\n"]
    [caloc () "(call allocate 2)\n"]
    [caerr () "(call array-error 2)\n"]
    [retun () "(return)\n"]))

(define (format-arop arop)
  (type-case Aop arop
    [addop () "+="]
    [subop () "-="]
    [mulop () "*="]
    [andop () "&="]))

(define (format-sfop sfop)
  (type-case Sop sfop
    [sflft () "<<="]
    [sfrht () ">>="]))

(define (format-comp comp)
  (type-case Cmp comp
    [less () "<"]
    [leeq () "<="]
    [eqal () "="]))

;(require racket/cmdline)
;(define file-to-compile
;  (command-line
;   #:args (filename)
;   filename))

;========== MAIN ==========
(define in (open-input-file "2-test/9.L2"))
(define l2p (parsep (read in)))
;(define l2f (first (prog-funl l2p)))
;(println (func-insl l2f))
;(display-inout (in&out (func-insl l2f)))
;(define g (build-graph (func-insl l2f)))
;(display-graph g)
(display-prog (l2-compiler l2p))