#lang racket
(require "Utility.rkt")
;(parser 'a) -> (var-exp a)
;(parser '(function (x) x)) -> (func-exp ((var-exp x)) (var-exp x))
;(parser '(call (function (x) x) a)) -> (app-exp (func-exp ((var-exp x)) (var-exp x)) (var-exp a))
(define parser
  (lambda (statement)
    (cond
      ((symbol? statement) (list 'var-exp statement)) ;this is a variable expression
      ((number? statement) (list 'num-exp statement))
      ;bool: (operator op1 op2) -> (bool-exp operator (exp op1) (exp op2))
      ((and
        (list? statement)
        (check_bool_op (car statement))
        (eq? (length statement) 3)
       )
       (list 'bool-exp (car statement) (parser (cadr statement)) (parser (caddr statement)))
       )
      ((and
        (list? statement)
        (eq? '! (car statement))
        (eq? (length statement) 2)
        )
       (list 'bool-exp (car statement) parser (cadr statement))
       )
      ;math expression: (operator op1 op2) -> (math-exp operator (exp op1) (exp op2))
      ((and
        (list? statement)
        (check_math_op (car statement))
        (eq? (length statement) 3)
        )
        (list
         'math-exp
         (car statement)
         (parser (cadr statement))
         (parser (caddr statement)))
        )
      ((and
        (list? statement)
        (eq? 'function (car statement))
        (eq? (length statement) 3)
        )
       (list 'func-exp (list (parser (cadr statement))) (parser (caddr statement)))
       )
      ;this is a function expression
      ;(function (arg) body)
      ((and
        (list? statement)
        (eq? 'call (car statement))
        (eq? (length statement) 3)
        )
       ;(call (function (x y) (* x y)) (5 c))
       ;check the parameter number matches with the values passed in
       (if
        (eq? (length (cadr (cadr statement))) (length (caddr statement)))
        (list 'app-exp (parser (cadr statement)) (parser (caddr statement)))
        (error-output "Error: argument list mismatches.")
       )
       );this is an app epxression
       ((and
        (list? statement)
        (eq? 'let (car statement))
        (eq? (length statement) 3))
       (let ((bindings (cadr statement))
             (body (caddr statement)))
                (list 'let-exp
             (cons
              'list-exp
              (map
               (lambda (pair)
                 (map (lambda (item) (parser item)) pair))
               (cadr statement))
              )
             (parser (caddr statement)))
       )
       )
             ((and
        (pair? statement)
        (eq? 'assign (car statement))
        (eq? (length statement) 3))
       (list
        'assign-exp
        (list
         (parser (cadr statement))
         (parser (caddr statement)))
        )
       );this is assign expression to update/create one variable
      ((and
        (list? statement)
        (eq? 'ask (car statement))
        (eq? (length statement) 4))
        (list
         'ask-exp
         (parser (cadr statement))
         (parser (caddr statement))
         (parser (cadddr statement))
         )
        );this is an ask expression
            ((and
        (pair? statement)
        (eq? 'out (car statement))
        (eq? (length statement) 2)
        )
       (list 'output-exp (parser (cadr statement))));this is an output expression
                  ((and
        (pair? statement)
        (eq? 'when (car statement))
        (eq? (length statement) 3))
       (cons
        'when-exp
        (map (lambda (item) (parser item)) (cdr statement)))
       );this is our when expression (while)
                  ((and
  (pair? statement)
  (eq? 'while (car statement))
  (eq? (length statement) 3))
  (list 'while-exp (parser (cadr statement)) (parser (caddr statement))))
      ((and
        (pair? statement)
        (eq? 'block (car statement))
        (> (length statement) 1))
       (cons 'block-exp
             (map (lambda(item)(parser item)) (cdr statement))));this is a block expression contains multiple statements in the list
      
      ((list? statement) ;(x 1 z ...) -> (list-exp (var-exp x) (num-exp 1) (var-exp z) ...)
       (cons 'list-exp (map (lambda(item)
              (parser item)
              ) statement))
       );this is a list expression

      ;this is let expression to add new local variables
      (else
       (error-output "Parsing failed. Unknown statement."))
      )
    )
  )

(define check_bool_op
  (lambda (op)
    (cond
      ((eq? op '>) #t)
      ((eq? op '<) #t)
      ((eq? op '>=) #t)
      ((eq? op '<=) #t)
      ((eq? op '==) #t)
      ((eq? op '&&) #t)
      ((eq? op '||) #t)
      ((eq? op '!) #t)
      ((eq? op '!=) #t)
      (else #f)
      )
    )
  )

(define check_math_op
  (lambda (op)
    (is_in_list (list '+ '- '* '/ '// '%) op)
    )
  )

    
(provide (all-defined-out))