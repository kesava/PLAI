type rec exprC =
  | NumC(int)
  | IdC(string)
  | AppC(string, exprC)
  | PlusC(exprC, exprC)
  | MultC(exprC, exprC)
  | IfCondC(exprC, exprC, exprC)

type name = string
type arg = string
type body = exprC
type funDefC = 
  | FdC(name, arg, body)

let rec getFunDef = (f: string, fds: list<funDefC>) => {
  switch fds {
  | list{} => raise(Not_found)
  | list{a, ...rest} => {
    switch a {
      | FdC(fname, _, _) => if (f === fname) { a } else { getFunDef(f, rest) };
    }
    }
  }
}

let fdCarg = fd => {
  switch fd {
  | FdC(_, a, _) => a
  }
}

let fdCbody = fd => {
  switch fd {
  | FdC(_, _, b) => b
  }
}

let rec subst = (wat: exprC, forr: string, inn: exprC) => {
  switch inn {
  | NumC(_) => inn;
  | IdC(sym) => if (sym === forr) { wat } else { inn };
  | PlusC(l, r) => PlusC(subst(wat, forr, l), subst(wat, forr, r));
  | MultC(l, r) => MultC(subst(wat, forr, l), subst(wat, forr, r));
  | IfCondC(p, c, a) => IfCondC(subst(wat, forr, p), subst(wat, forr, c), subst(wat, forr, a));
  | AppC(f, a) => AppC(f, subst(wat, forr, a));
  ;
  }
}

let rec interp = (exp, fds) => {
  switch exp {
    | NumC(n) => n;
    | IdC(_) => raise(Not_found);
    | AppC(f, a) => {
      let fd = getFunDef(f, fds);
      interp(subst(a, fdCarg(fd), fdCbody(fd)), fds);
    };
    | PlusC(l, r) => interp(l, fds) + interp(r, fds);
    | MultC(l, r) => interp(l, fds) * interp(r, fds);
    | IfCondC(p, c, a) => if (interp(p, fds) == 0) { interp(a, fds) } else { interp(c, fds) };
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
let fd1 = FdC("double", "x", PlusC(IdC("x"), NumC(5)));
let fd2 = FdC("quad", "x", AppC("double", AppC("double", IdC("x"))));
let fd3 = FdC("const5", "_", NumC(5));

let an = interp(PlusC(MultC(AppC("double", NumC(2)), NumC(4)), NumC(3)), list{fd1, fd2, fd3});

Js.log(an);
// 31

let an1 = interp(desugar(PlusS(SquareS(NumS(8)), NumS(3))), list{fd1, fd2, fd3});

