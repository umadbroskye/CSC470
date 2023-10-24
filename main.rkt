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



(parser '(call (function (x y) (* x y)) (5 3)))
(processor (parser '(call (function (x y) (* x y)) (b c))) var_env)