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


(processor (parser '(call (function(x)(ask (== a 1) (+ x 1) (- x 1))) (2))) var_env)
