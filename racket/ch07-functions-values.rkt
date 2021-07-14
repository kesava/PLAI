
#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  ;; [ifCondC (pred : ExprC) (conseq : ExprC) (alter : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define-type Binding
  [bind (name : symbol) (value : Value)])

(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

(define-type-alias Env (listof Binding))
(define mt-Env empty)
(define extend-env cons)


(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [fdC (n a b) (funV n a b)]
    [appC (f a) (local ([define fd (interp f env)])
                               (interp (funV-body fd) (extend-env (bind (funV-arg fd)
                                                          (interp a env))
                                                    mt-Env)))]
                  ;;[else (error 'interp "function definition is incorrect")])]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    ;; [ifCondC (p c a) (if (>= (interp p env) 0) (interp c env) (interp a env))]
    [multC (l r) (num* (interp l env) (interp r env))]))

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "at least one argument was not a number.")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num* "at least one argument was not a number.")]))

(define (lookup [n : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup (string-append (symbol->string n) " - variable not found"))]
    [else (cond
            [(symbol=? n (bind-name (first env))) (bind-value (first env))]
            [else (lookup n (rest env))])]))

(test (interp (plusC (numC 2) (numC 3)) mt-Env) (numV 5))
(test (interp (multC (plusC (idC 'x) (numC 3)) (numC 4)) (extend-env (bind 'x (numV 5)) mt-Env)) (numV 32))
(test (interp (appC (fdC 'double 'x (plusC (idC 'x) (idC 'x))) (numC 10)) (extend-env (bind 'x (numV 5)) mt-Env)) (numV 20))