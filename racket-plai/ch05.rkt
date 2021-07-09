
#lang plai-typed

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [ifCondC (pred : ExprC) (conseq : ExprC) (alter : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define fd1 (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
(define fd2 (fdC 'quad 'x (appC 'double (appC 'double (idC 'x)))))
(define fd3 (fdC 'const5 '_ (numC 5)))

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l) (subst what for r))]
    [ifCondC (p c a) (ifCondC (subst what for p) (subst what for c) (subst what for a))]
    [multC (l r) (multC (subst what for l) (subst what for r))]))

(define (interp [a : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst a (fdC-arg fd) (fdC-body fd)) fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [ifCondC (p c a) (if (>= (interp p fds) 0) (interp c fds) (interp a fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to an undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

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
    (interp (desugar a) (list fd1 fd2 fd3)))


(test (interp (plusC (numC 2) (numC 3)) (list fd1)) 5)
(test (interp (multC (plusC (numC 2) (numC 3)) (numC 4)) (list fd1)) 20)
(test (interp (appC 'double (numC 4)) (list fd1)) 8)
(test (interp (appC 'quad (numC 4)) (list fd1 fd2)) 16)
