
#lang plai-typed

(define-type FunDefC
  [fdC (name : symbol) (args : (listof symbol)) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (args : (listof ExprC))]
  [plusC (l : ExprC) (r : ExprC)]
  [ifCondC (pred : ExprC) (conseq : ExprC) (alter : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define-type Binding
  [bind (name : symbol) (value : number)])

(define-type-alias Env (listof Binding))
(define mt-Env empty)
(define extend-env cons)

(define fd1 (fdC 'double (list 'x) (plusC (idC 'x) (idC 'x))))
(define fd2 (fdC 'quad (list 'x) (appC 'double (list (appC 'double (list (idC 'x)))))))
(define fd3 (fdC 'const5 (list '_) (numC 5)))
(define fd4 (fdC 'add (list 'x 'y) (plusC (idC 'x) (idC 'y))))


(define (extendEnvParamsArgs [params : (listof symbol)] [args : (listof ExprC)] [env : Env] [fds : (listof FunDefC)]) : Env
  (cond
    [(or (empty? params) (empty? args)) env]
    (else (extend-env (bind (first params)  (interp (first args) env fds)) (extendEnvParamsArgs (rest params) (rest args) env fds)))))

(define (interp [a : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f args) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd) (extendEnvParamsArgs (fdC-args fd) args env fds) fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [ifCondC (p c a) (if (>= (interp p env fds) 0) (interp c env fds) (interp a env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to an undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define (lookup [n : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? n (bind-name (first env))) (bind-value (first env))]
            [else (lookup n (rest env))])]))

(define (parseS [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         ['+ (plusS (parseS (second sl)) (parseS (third sl)))]
         ['* (multS (parseS (second sl)) (parseS (third sl)))]
         ['- (if (= (length sl) 2)
                    (uminusS (parseS (second sl)))
                    (bminusS (parseS (second sl)) (parseS (third sl))))]
         ['if (ifCondS (parseS (second sl)) (parseS (third sl)) (parseS (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (n : ArithS)]
  [ifCondS (pred : ArithS) (conseq : ArithS) (alter : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)])

(define (desugar [as : ArithS]) : ExprC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (n) (desugar (bminusS (numS 0) n))]
    [ifCondS (p c a) (ifCondC (desugar p) (desugar c) (desugar a))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

(define (interpS [a : ArithS]) : number
    (interp (desugar a) mt-Env (list fd1 fd2 fd3)))


(test (interp (plusC (numC 2) (numC 3)) mt-Env (list fd1)) 5)
(test (interp (multC (plusC (idC 'x) (numC 3)) (numC 4)) (extend-env (bind 'x 5) mt-Env) (list fd1)) 32)
(test (interp (appC 'double (list (idC 'x))) (extend-env (bind 'x 5) mt-Env ) (list fd1)) 10)
(test (interp (appC 'quad (list (idC 'x))) (extend-env (bind 'x 5) mt-Env) (list fd1 fd2)) 20)
(test (interp (appC 'add (list (idC 'x) (idC 'y))) (extend-env (bind 'x 5) (extend-env (bind 'y 95) mt-Env)) (list fd1 fd2 fd4)) 100)

