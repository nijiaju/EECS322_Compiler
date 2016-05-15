#lang plai

(require "l4-parser.rkt" "l4-compiler.rkt" "../l3c/l3-format.rkt")

(require racket/cmdline)
(define file-to-compile
  (command-line
   #:args (filename)
   filename))

;========== MAIN ==========
;(define in (open-input-file "test"))
(define in (open-input-file file-to-compile))
(define l4prog (l4-parsp (read in)))
(displayln (format-l3-prog (l4-compile-prog l4prog)))
