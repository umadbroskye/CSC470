#lang racket
(require "Utility.rkt")
(require "Parser.rkt")
(require "Processor.rkt")

(define var_env
  '(;environment
   (;global variable scope
    (a 1) (b 2) (c 3)
    )
   )
  )

(processor (parser '(call (function (a) (call (function (r) a ) (a))) (5))) var_env)
; should return 1
