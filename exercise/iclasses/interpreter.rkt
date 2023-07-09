#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)
(define-struct class-descriptor (name superclass fields methods env))
(define-struct method (params body))
(define-struct object (class fields))

(define (value-of exp env)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 env) (value-of e2 env))]
    [(ast:zero? e) (zero? (value-of e env))]
    [(ast:not e) (value-of e env)]
    [(ast:if e1 e2 e3) (if (value-of e1 env) (value-of e2 env) (value-of e3 env))]
    [(ast:var v) (apply-env env v)]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env env x (value-of e1 env)))]
    [(ast:send e (ast:var mth) args)
     (let* ([obj (value-of e env)]
            [class (object-class obj)]
            [method (lookup-method class mth)])
       (let ([method-env (extend-env (class-env class) 'self obj)])
         (result-of method args method-env)))
     ]
    [(ast:super (ast:var c) args)
     (error "Super expression unimplemented")]
    [(ast:self) (error "Self expression unimplemented")]
    [(ast:new (ast:var c) args)
     (let* ([class (apply-env env c)]
            [fields (map (lambda (arg) (value-of arg env)) args)]
            [obj (make-object class fields)])
       obj)
     ]
    [e (error (format "value-of: unimplemented-construction: ~a" e))]
    ))

(define (result-of stmt env)
  (match stmt
    [(ast:assign (ast:var x) e) (extend-env env x (value-of e env))]
    [(ast:print e) (display (value-of e env))
                   (newline)
                   env]
    [(ast:return e) (value-of e env)]
    [(ast:block stmts) (foldl result-of env stmts)]
    [(ast:if-stmt e s1 s2) (if (value-of e env)
                              (result-of s1 env)
                              (result-of s2 env))]
    [(ast:while e s) (error "While statement unimplemented")]
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env env x 'uninitialized))]
    [(ast:send e (ast:var mth) args)
     (error "Command send unimplemented")]
    [(ast:super (ast:var c) args)
     (error "Command super unimplemented")]
    [e (error (format "result-of: unimplemented-construction: ~a" e))]
    ))

(define (lookup-method class method-name)
  (let loop ([cls class])
    (if cls
        (let* ([methods (class-methods cls)]
               [method (assoc method-name methods)])
          (if method
              (cdr method)
              (loop (class-super cls))))
        (error "Method not found"))))

(define (value-of-program prog)
  (match prog
    [(ast:prog decls stmt)
     (let* ([Δ (empty-store)]
            [env (foldl process-class Δ decls)])
       (result-of stmt env))
     ]))

(define (process-class Δ decl)
  (match decl
    [(ast:decl (ast:var cname) (ast:var super) fields methods)
     (let* ([fields-list (map car fields)]
            [methods-list (map (lambda (method-decl)
                                 (method (cadr method-decl) (caddr method-decl)))
                               methods)]
            [class-env (extend-env (empty-env) 'self (object class-descriptor 'self fields-list methods-list (empty-env)))]
            [class (class-descriptor cname super fields-list methods-list class-env)])
       (extend-env Δ cname class)
       )
     ]
    [else (error "Invalid declaration")])
  )

(define (class-env class)
  (class-descriptor-env class))

(define (class-methods class)
  (class-descriptor-methods class))

#;(define (class-descriptor-methods class)
  (class-methods class))

(define (class-super class)
  (class-descriptor-super class))

(define (class-descriptor-super class)
  (class-super class))
