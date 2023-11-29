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


(define code '(each (a 0) (> 5 a) (a (+ a 1)) ((out a))))


(define parsed (parser code))
parsed

(processor parsed var_env)