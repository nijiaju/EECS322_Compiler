#lang plai

(require "l2-definition.rkt")
(require graph)

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
  (cond
    [(string? p) (println p)]
    [(prog? p) (printf "(~a\n~a)\n" (prog-name p)
                       (foldr string-append "" (map format-func (prog-funl p))))]
    [else (error 'display-prog "unknown error")]))

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
    [cread () "(call read 0)\n"]
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