#lang plai

(require "l2-definition.rkt")
(require "l2-parser.rkt")
(require "graph.rkt")
(require "graph-format.rkt")

(require racket/cmdline)

(define file-to-compile
  (command-line
   #:args (filename)
   filename))

;========== MAIN ==========
(define in (open-input-file file-to-compile))
(define l2f (parsef (read in)))
;(println (func-insl l2f))
;(display-inout (in&out (func-insl l2f)))
(define g (build-graph (func-insl l2f)))
(display-graph g)
(display-res (coloring g))