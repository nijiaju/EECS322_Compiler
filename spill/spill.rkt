#lang plai

;========== TYPE DEFINITION ==========

(define-type Prog
  [prog (name islabel?)
        (funs (listof Func?))])

(define-type Func
  [func (name islabel?)
        (narg number?)
        (nspl number?)
        (inss (listof Inst?))])

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
    [(? islabel?)                (label sexp)]
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

;(define sorce_code (call-with-input-file "test" read))
(define in (open-input-file "test"))
(define sorce_code (parsef (read in)))
(println sorce_code)
(println (read in))
(println (read in))
;(println (read "test"))
;(println (parsep sorce_code))


