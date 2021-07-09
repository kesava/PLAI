type rec arithC =
  | NumC(int)
  | PlusC(arithC, arithC)
  | MultC(arithC, arithC)
  | IfCondC(arithC, arithC, arithC)


let rec interp = (exp) => {
  switch exp {
    | NumC(n) => n;
    | PlusC(l, r) => interp(l) + interp(r);
    | MultC(l, r) => interp(l) * interp(r);
    | IfCondC(p, c, a) => if (interp(p) == 0) { interp(a) } else { interp(c) };
  }
}

type rec arithS = 
| NumS(int)
| PlusS(arithS, arithS)
| MultS(arithS, arithS)
| MinusS(arithS, arithS)
| SquareS(arithS)
| IfCondS(arithS, arithS, arithS)


let rec desugar = (ars) => {
  switch ars {
    | NumS(n) => NumC(n);
    | PlusS(l, r) => PlusC(desugar(l), desugar(r));
    | MultS(l, r) => MultC(desugar(l), desugar(r));
    | MinusS(l, r) => PlusC(desugar(l), MultC(NumC(-1), desugar(r)));
    | SquareS(l) => PlusC(desugar(l), desugar(l));
    | IfCondS(p, c, a) => IfCondC(desugar(p), desugar(c), desugar(a))
  }
}

let an = interp(PlusC(MultC(NumC(2), NumC(4)), NumC(3)));

let an1 = interp(desugar(PlusS(SquareS(NumS(8)), NumS(3))));

type rec inputList =
	| Str(string)
	| Lt(array<inputList>)



let numFromStr = str => switch Belt.Int.fromString(str) {
  | None => 0;
  | Some(n) => n;
}

let a = numFromStr("22")