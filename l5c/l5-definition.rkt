#lang plai

(define-type L5e
  [l5lambda (vars (listof l5var?)) (body L5e?)]
  [l5let    (var l5var?) (value L5e?) (body L5e?)]
  [l5letrec (var l5var?) (value L5e?) (body L5e?)]
  [l5if     (cond L5e?) (then L5e?) (else L5e?)]
  [l5newtup (vals (listof L5e?))]
  [l5begin  (exp1 L5e?) (exp2 L5e?)]
  [l5call   (func L5e?) (args (listof L5e?))]
  [l5num    (num number?)]
  [l5prim   (prim is-l5prim)]
  [l5var    (var is-l5var)])

(define-type L5pred
  [l5number?]
  [l5array?])

(define (is-l5var v)
  (match v
    [(? is-l5prim) #f]
    [(app symbol->string (regexp #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$")) #t]
    [_ (println "hello")]))

(define (is-l5prim v)
  (match v
    [(or 'read 'print 'new-array 'aref 'aset 'alen) #t]
    [(? is-l5biop) #t]
    [(? is-l5pred) #t]
    [_ #f]))

(define (is-l5biop op)
  (match op
    [(or '+ '- '* '< '<= '=) #t]
    [_ #f]))

(define (is-l5pred pred)
  (match pred
    [(or 'number? 'a?) #t]
    [_ #f]))

(define (unwrap-l5var var)
  (type-case L5e var
    [l5var (var) var]
    [else (error 'unwrap-l5var)]))