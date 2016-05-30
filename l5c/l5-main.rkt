#lang plai

(require "l5-parser.rkt" "l5-compiler.rkt" "../l4c/l4-format.rkt")

(require racket/cmdline)
(define file-to-compile
  (command-line
   #:args (filename)
   filename))

;========== MAIN ==========
;(define in (open-input-file "../interps/tests/robby/5-test/27.L5"))
(define in (open-input-file file-to-compile))
(define l5expr (l5parse (read in)))
(displayln (format-l4-prog (l5-compiler l5expr empty)))