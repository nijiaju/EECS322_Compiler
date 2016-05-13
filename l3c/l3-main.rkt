#lang plai

(require "l3-parser.rkt" "l3-compiler.rkt" "../l2c/l2-format.rkt")

(require racket/cmdline)
(define file-to-compile
  (command-line
   #:args (filename)
   filename))

;========== MAIN ==========
;(define in (open-input-file "../interps/tests/robby/3-test/90.L3"))
(define in (open-input-file file-to-compile))
(define l3prog (l3-parsp (read in)))
(display-prog (l3-compile-prog l3prog))