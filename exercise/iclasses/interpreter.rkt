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
    [(ast:send e (ast:var mth) args)
     (let* ([obj (value-of e Δ)]
            (println obj)
            [class (object-class obj)]
            [method (lookup-method class mth)])
       (let ([method-env (extend-env (class-env class) 'self obj)])
         (result-of method args method-env)))
     ]
    [(ast:super (ast:var c) args) (display "super expression unimplemented")]
    [(ast:self) (display "self expression unimplemented")]
    [(ast:new (ast:var c) args)
     (let* ([class (apply-env Δ c)]
            [fields (map (lambda (arg) (value-of arg Δ)) args)]
            [obj (make-object class fields)])
       obj)
     ]
    [e (raise-user-error "value-of: unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (var x) e) (extend-env x (value-of e Δ) Δ)]
    [(ast:print e) (display (value-of e Δ))
                   #;(display "print unimplemented")]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (foldl (lambda (lstmts lΔ)
                                (result-of lstmts lΔ)) Δ stmts)]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ)) #;(display "if statment unimplemented")]
    [(ast:while e s) (display "while unimplemented")]
    [(ast:local-decl (ast:var x) s) (result-of s x) ]
    [(ast:send e (ast:var mth) args)
     (error "Command send unimplemented")]
    [(ast:super (ast:var c) args) (display "command super unimplemented")]
    [e (raise-user-error "result-of: unimplemented-construction: " e)]
    ))

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       (let* ([Δ (empty-store)]
             [env (process-classes Δ decls)])
       ; you must collect all the classes declared and building its respectively environment
       ; execute the prog expression in the correct environment
       (result-of stmt env)))]))


; Func para buscar o que tem na classe
; valores i j para fazer o env e guardar num descritor de class
; 


;;;;;;;;;;;;;;;;;;;;; FUNCOES AUXILIARES ;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct class-descriptor (name superclass fields methods env) #:transparent)
(define-struct method (params body) #:transparent)
(define-struct object (class fields) #:transparent)



(define (lookup-method class method-name) 
  (let loop ([cls class])
    (if cls
        (let* ([methods (class-methods cls)]
               [method (assoc method-name methods)])
          (if method
              (cdr method)
              (loop (class-super cls))))
        (error "Method not found"))))

(define (process-classes Δ decls)
  (if (null? decls)
      Δ
      (let* ([decl (car decls)]
             [Δ' (process-class Δ decl)])
        (process-classes Δ' (cdr decls)))))


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
