#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (value-of e Δ)]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (apply-env Δ v)] ; esta implementação só funciona para variáveis imutáveis
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args) (display "send expression unimplemented")]
    [(ast:super (ast:var c) args) (display "super expression unimplemented")]
    [(ast:self) (display "self expression unimplemented")]
    [(ast:new (ast:var c) args) (display (string-append "new " c))]
    [e (raise-user-error "value-of: unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (extend-env x (value-of e Δ) Δ)]
    [(ast:print e) (display (value-of e Δ))
                   #;(display "print unimplemented")]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (foldl (lambda (lstmts lΔ)
                                (result-of lstmts lΔ)) Δ stmts)]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ)) #;(display "if statment unimplemented")]
    [(ast:while e s) (display "while unimplemented")]
    [(ast:local-decl (ast:var x) s) (result-of s x) ]
    [(ast:send e (ast:var mth) args) (display "command send unimplemented")]
    [(ast:super (ast:var c) args) (display "command super unimplemented")]
    [e (raise-user-error "result-of: unimplemented-construction: " e)]
    ))

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       ;funcao pra buscar no decls o que tem na classe
       ;(display stmt)
       
       ; you must collect all the classes declared and building its respectively environment
       ; execute the prog expression in the correct environment
       (result-of stmt init-env))]))


; Func para buscar o que tem na classe
; valores i j para fazer o env e guardar num descritor de class
; 



(struct class-desciptor (name super fields env) #:transparent)
(struct method (params body) #:transparent)
(struct object (class fields) #:transparent)


