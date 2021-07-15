
#lang plai-typed


(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  ;; [ifCondC (pred : ExprC) (conseq : ExprC) (alter : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define-type Binding
  [bind (name : symbol) (value : Value)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type-alias Env (listof Binding))
(define mt-Env empty)
(define extend-env cons)


(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [lamC (a b) (closV a b env)]
    [appC (f a) (local ([define f-value (interp f env)])
                  (cond
                    [(closV? f-value) (interp (closV-body f-value) (extend-env (bind (closV-arg f-value)
                                                          (interp a env))
                                                   (closV-env f-value)))]
                  [else (error 'interp "function definition is incorrect")]))]
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
(test (interp
       (appC (lamC 'x (plusC (idC 'x) (numC 6))) (numC 4))
       mt-Env)
      (numV 10))
(test (interp
         (appC (appC (lamC 'x
                   (lamC 'y
                        (plusC (idC 'x) (idC 'y))))
              (numC 4)) (numC 5))
       mt-Env)
      (numV 9))