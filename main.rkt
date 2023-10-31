#lang racket
(require "Utility.rkt")
(require "Parser.rkt")
(require "Processor.rkt")

(define var_env
  '(;environment
   (;global variable scope
    (a 1) (b 3) (c 5)
    )
   )
  )

(processor (parser '(call (function(x)(let ((a 3) (b 4)) (+ a (+ x b))))(5))) var_env)
; should return 12
