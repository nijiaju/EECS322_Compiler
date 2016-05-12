#lang plai

(require "L3-parser.rkt" "L3-compiler.rkt" "../l2c/l2-format.rkt")

(require racket/cmdline)
(define file-to-compile
  (command-line
   #:args (filename)
   filename))

;========== MAIN ==========
(define in (open-input-file "test/tests1.L3"))
(define l3prog (l3-parsp (read in)))
(display-prog (l3-compile-prog l3prog))