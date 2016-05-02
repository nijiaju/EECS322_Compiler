#lang plai

(require "l2-definition.rkt")

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