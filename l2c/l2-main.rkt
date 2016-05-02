#lang plai

(require "l2-format.rkt")
(require "l2-parser.rkt")
(require "l2-compiler.rkt")

(require racket/cmdline)
(define file-to-compile
  (command-line
   #:args (filename)
   filename))

;========== MAIN ==========
(define in (open-input-file file-to-compile))
(define l2p (parsep (read in)))
;(define l2f (first (prog-funl l2p)))
;(println (func-insl l2f))
;(display-inout (in&out (func-insl l2f)))
;(define g (build-graph (func-insl l2f)))
;(display-graph g)
(display-prog (l2-compiler l2p))