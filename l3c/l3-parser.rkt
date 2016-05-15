#lang plai

(require "l3-definition.rkt" "../l2c/l2-definition.rkt")

(define (l3-parsp sexp)
  (match sexp
    [`(,e1 ,e2 ...) (l3prog (l3-parse e1) (map l3-parsf e2))]
    [_ (error 'l3-parsp "syntax error")]))

(define (l3-parsf sexp)
  (match sexp
    [`(,e1 (,e2 ...) ,e3) (l3func e1 e2 (length e2) (l3-parse e3))]
    [_ (error 'l3-parsf "syntax error")]))

(define (l3-parse sexp)
  (match sexp
    [`(let ([,e1 ,e2]) ,e3) (l3lete (l3-parsv e1) (l3-parsd e2) (l3-parse e3))]
    [`(if ,e1 ,e2 ,e3) (l3ife (l3-parsv e1) (l3-parse e2) (l3-parse e3))]
    [_ (l3defe (l3-parsd sexp))]))

(define (l3-parsd sexp)
  (match sexp
    [`(+ ,e1 ,e2)   (l3biop (addop) (l3-parsv e1) (l3-parsv e2))]
    [`(- ,e1 ,e2)   (l3biop (subop) (l3-parsv e1) (l3-parsv e2))]
    [`(* ,e1 ,e2)   (l3biop (mulop) (l3-parsv e1) (l3-parsv e2))]
    [`(< ,e1 ,e2)   (l3biop (less) (l3-parsv e1) (l3-parsv e2))]
    [`(<= ,e1 ,e2)  (l3biop (leeq) (l3-parsv e1) (l3-parsv e2))]
    [`(= ,e1 ,e2)   (l3biop (eqal) (l3-parsv e1) (l3-parsv e2))]
    [`(number? ,e1) (l3pred (numpred) (l3-parsv e1))]
    [`(a? ,e1)      (l3pred (arrpred)  (l3-parsv e1))]
    [`(new-array ,e1 ,e2)    (l3newarr (l3-parsv e1) (l3-parsv e2))]
    [`(new-tuple ,e1 ...)    (l3newtup (map l3-parsv e1))]
    [`(aref ,e1 ,e2)         (l3arrref (l3-parsv e1) (l3-parsv e2))]
    [`(aset ,e1 ,e2 ,e3)     (l3arrset (l3-parsv e1) (l3-parsv e2) (l3-parsv e3))]
    [`(alen ,e1)             (l3arrlen (l3-parsv e1))]
    [`(print ,e1)            (l3printd (l3-parsv e1))]
    [`(make-closure ,e1 ,e2) (l3makecl e1 (l3-parsv e2))]
    [`(closure-proc ,e1)     (l3clproc (l3-parsv e1))]
    [`(closure-vars ,e1)     (l3clvars (l3-parsv e1))]
    [`(,e1 ,e2 ...) (l3funcal (l3-parsv e1) (map l3-parsv e2))]
    [`,e1           (l3value  (l3-parsv e1))]
    [_              (error 'l3-parsd "syntax error")]))

(define (l3-parsv sexp)
  (match sexp
    [(? number?)      (l3numbe (encode sexp))]
    [(? is-label?)    (l3label sexp)]
    [(? is-variable?) (l3varia sexp)]
    [_  (error 'l3-parsv "syntax error")]))

(define (encode n) (+ (* n 2) 1))

(define l3-var-counter (box 0))
(define l4-var-counter (box 0))

(define (get-var-suffix lang)
  (let
      ([counter (match lang
                  ['L3 l3-var-counter]
                  ['L4 l4-var-counter]
                  [_ (error 'get-label-suffix)])])
    (begin0 (number->string (unbox counter))
            (set-box! counter (+ (unbox counter) 1)))))

(define (var-suffix var lang)
  (string->symbol (string-append (symbol->string var) (get-var-suffix lang))))

(define l3-lab-counter (box 0))
(define l4-lab-counter (box 0))

(define (get-label-suffix lang)
  (let
      ([counter (match lang
                  ['L3 l3-lab-counter]
                  ['L4 l4-lab-counter]
                  [_ (error 'get-label-suffix)])])
    (begin0 (number->string (unbox counter))
            (set-box! counter (+ (unbox counter) 1)))))

(define (label-suffix lab lang)
  (string->symbol (string-append (symbol->string lab) (get-label-suffix lang))))

(define (l3-preprocesser p)
  (l3prog (l3-exp-preprocessor (l3prog-l3entry p) (hash))
          (map l3-func-preprocessor (l3prog-l3funcl p))))

(define/contract (l3-func-preprocessor f)
  (-> l3func? l3func?)
  (define l3context (l3-context-init (l3func-l3fvarl f) (hash)))
  (l3func (l3func-l3fname f)
          (map (λ (v) (unwrap-var (l3-val-preprocessor (l3varia v) l3context))) (l3func-l3fvarl f))
          (l3func-l3nargs f)
          (l3-exp-preprocessor (l3func-l3fbody f) l3context)))

(define/contract (l3-exp-preprocessor e c)
  (-> L3Expression? hash? L3Expression?)
  (type-case L3Expression e
    [l3lete (var def exp)
            (let ([c2 (hash-set c (unwrap-var var) (var-suffix 'l3_x_ 'L3))])
              (l3lete (l3varia (hash-ref c2 (unwrap-var var)))
                      (l3-def-preprocessor def c)
                      (l3-exp-preprocessor exp c2)))]
    [l3ife  (cond then else)
            (l3ife (if (l3varia? cond) (l3varia (hash-ref c (unwrap-var cond))) cond)
                   (l3-exp-preprocessor then c)
                   (l3-exp-preprocessor else c))]
    [l3defe (d) (l3defe (l3-def-preprocessor d c))]))

(define/contract (l3-def-preprocessor d c)
  (-> L3Definition? hash? L3Definition?)
  (type-case L3Definition d
    [l3biop (oper lhs rhs)
            (l3biop oper
                    (l3-val-preprocessor lhs c)
                    (l3-val-preprocessor rhs c))]
    [l3pred (oper valu)
            (l3pred oper (l3-val-preprocessor valu c))]
    [l3funcal (name argl)
              (l3funcal (l3-val-preprocessor name c)
                        (map (λ (v) (l3-val-preprocessor v c)) argl))]
    [l3newarr (size valu)
              (l3newarr (l3-val-preprocessor size c)
                        (l3-val-preprocessor valu c))]
    [l3newtup (vall) (l3newtup (map (λ (v) (l3-val-preprocessor v c)) vall))]
    [l3arrref (array posi)
              (l3arrref (l3-val-preprocessor array c)
                        (l3-val-preprocessor posi c))]
    [l3arrset (array posi valu)
              (l3arrset (l3-val-preprocessor array c)
                        (l3-val-preprocessor posi c)
                        (l3-val-preprocessor valu c))]
    [l3arrlen (array) (l3arrlen (l3-val-preprocessor array c))]
    [l3printd (valu) (l3printd (l3-val-preprocessor valu c))]
    [l3makecl (name vars)
              (l3makecl name (l3-val-preprocessor vars c))]
    [l3clproc (clos) (l3clproc (l3-val-preprocessor clos c))]
    [l3clvars (vars) (l3clvars (l3-val-preprocessor vars c))]
    [l3value (value) (l3value (l3-val-preprocessor value c))]))

(define/contract (l3-val-preprocessor v c)
  (-> L3Value? hash? L3Value?)
  (type-case L3Value v
    [l3varia (var) (l3varia (hash-ref c var))]
    [else v]))

(define/contract (l3-context-init argl h)
  (-> (listof is-variable?) hash? hash?)
  (cond
    [(empty? argl) h]
    [else (hash-set (l3-context-init (rest argl) h) (first argl) (var-suffix 'l3_x_ 'L3))]))

(define/contract (unwrap-var v)
  (-> L3Value? is-variable?)
  (type-case L3Value v
    [l3varia (var) var]
    [else (error 'unwrap-var)]))