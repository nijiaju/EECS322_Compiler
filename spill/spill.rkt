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

(define/contract (adjust-assign ins-list type)
  (-> (listof Inst?) symbol? (listof Inst?))
  (cond
    [(cons? (rest ins-list))
     (cons (first ins-list (adjust-assign (rest ins-list) type)))]
    [(empty? (rest ins-list))
     (cond
       [(eq? type 'mem-mem)
    

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
                       (let ([count (begin0 (unbox c) (set-box! c (+ (unbox c) 1)))])
                         (list (movei (new-var p count) (loadi (regst 'rsp) (* n 8)))
                               (loadi (new-var p count) offs)))]
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
                                         (movei (loadi (regst 'rsp) (* n 8)) (varia vars)))])]
                         [loadi (lsrc loff)
                                
                         [else (error 'spill "syntax error ~a" i)])]
                      [else (error 'spill "syntax error ~a" i)])]
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