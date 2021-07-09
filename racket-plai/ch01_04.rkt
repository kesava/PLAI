
#lang plai-typed
(define-type MisspelledAnimal
    [caml (humps : number)]
    [yacc (height : number)])

(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
    [caml (hum) (> hum 2)]
    [yacc (yc) (> yc 2.2)]))

(define ma1 (caml 3))
(define ma2 (yacc 2.3))

(test (good? ma1) #t)
(test (good? ma2) #t)

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [ifCondC (pred : ArithC) (conseq : ArithC) (alter : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         ['+ (plusC (parse (second sl)) (parse (third sl)))]
         ['* (multC (parse (second sl)) (parse (third sl)))]
         ['if (ifCondC (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [ifCondC (p c a) (if (>= (interp p) 0) (interp c) (interp a))]
    [multC (l r) (* (interp l) (interp r))]))

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

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (n) (desugar (bminusS (numS 0) n))]
    [ifCondS (p c a) (ifCondC (desugar p) (desugar c) (desugar a))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

(define (interpS [a : ArithS]) : number
    (interp (desugar a)))