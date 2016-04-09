#lang plai

;========== TYPE DEFINITION ==========

(define-type Prog
  [prog (name string?)
        (funs (listof Func?))])

(define-type Func
  [func (name string?)
        (narg number?)
        (nspl number?)
        (inss (listof Inst?))])

(define-type Inst
  [numbr (numb number?)]
  [regst (regs symbol?)]
  [label (labl string?)]
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

;========== PARSER ==========

(define (parsep sexp)
  (match sexp
    [`(,e1 ,e2 ...) (prog (substring (symbol->string e1) 1) (map parsef e2))]
    [_ (error 'parsep "syntax error")]))

(define (parsef sexp)
  (match sexp
    [`(,e1 ,e2 ,e3 ,e4 ...) (func (substring (symbol->string e1) 1) e2 e3 (map parsei e4))]
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
    [(? islabel?)                (label (substring (symbol->string sexp) 1))]
    [(? isregister?)             (regst sexp)]
    [(? isvariable?)             (varia sexp)]
    [_                           (error 'parsei "syntax error")]))

(define (islabel? s)
  (match (symbol->string s)
    [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*") #t]
    [_ #f]))

(define (isvariable? s)
  (match (symbol->string s)
    [(regexp #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$") #t]
    [_ #f]))

(define (isregister? s)
  (match (symbol->string s)
    [(regexp #rx"^(rdi|rsi|rdx|rcx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp)$") #t]
    [_ #f]))

;========== SPILL ==========

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
  (-> Func? symbol? symbol? Func?)
  (let ([counter (box 0)])
    (type-case Func f
      [func (name narg nspl inss)
            (func name narg (+ nspl 1)
                  (foldl
                   (λ (i res) (append res (spill i nspl counter v p)))
                   empty
                   inss))])))

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
                         [else (movei (loadi (regst 'rsp) (* n 8)) sorc)])]
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
    
    [else         (error 'spill "syntax error ~a" i)]))

(define in (open-input-file "test"))
(define l2function (parsef (read in)))
(println l2function)
(define var (read in))
(println var)
(define prefix (read in))
(println prefix)
(println (spill-func l2function var prefix))