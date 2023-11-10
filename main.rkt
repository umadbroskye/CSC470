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


(define code '(while (< a 4) ((assign a (+ a 1)) (out a))))

(define parsed (parser code))
parsed

(processor parsed var_env)