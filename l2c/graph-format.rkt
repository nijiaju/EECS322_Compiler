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

(define (display-res l)
  (cond
    [(equal? #f l) #f]
    [else (display "(") (for ([p l]) (printf "(~a ~a)" (car p) (cdr p))) (display ")")]))