type name = string
type arg = string

type rec exprC =
  | NumC(int)
  | IdC(string)
  | FdC(name, arg, exprC)
  | AppC(exprC, exprC)
  | PlusC(exprC, exprC)
  | MultC(exprC, exprC)
  // | IfCondC(exprC, exprC, exprC)

module Value = {
  type t =
  | NumV(int)
  | FunV(name, arg, exprC)

  let getNum = myType =>
    switch (myType) {
      | NumV(n) => n
      | _ => raise(Not_found)
    };

  let isNum = (x) : bool => 
    switch x {
      | NumV(_) => true
      | _ => false
    };
};

let fdCarg = (fd: Value.t) => {
  switch fd {
  | FunV(_, a, _) => a
  | _ => raise(Not_found);
  }
}

let fdCbody = (fd: Value.t) => {
  switch fd {
  | FunV(_, _, b) => b;
  | _ => raise(Not_found);
  }
}

type binding =
  | Bind(name, Value.t)

type env = list<binding>
let mtEnv: env = list{}
let extendEnv = Belt.List.add

let rec lookup = (n, env) => {
  switch env {
  | list{} => raise(Not_found)
  | list{a, ...rest} => {
    switch a {
    | Bind(k, v) => if (k === n) { v } else { lookup(n, rest)}
    }
    }
  }
}

let numPlus = (l: Value.t, r: Value.t) : Value.t => {
  if (Value.isNum(l) && Value.isNum(r)) {
    Value.NumV(Value.getNum(l) + Value.getNum(r));
  } else {
    raise(Not_found);
  }
}

let numMult = (l: Value.t, r: Value.t) : Value.t => {
  if (Value.isNum(l) && Value.isNum(r)) {
    Value.NumV(Value.getNum(l) * Value.getNum(r));
  } else {
    raise(Not_found);
  }
}

let rec interp = (exp, env): Value.t => {
  switch exp {
    | NumC(n) => NumV(n);
    | IdC(id) => lookup(id, env);
    | FdC(fname, arg, body) => FunV(fname, arg, body);
    | AppC(f, arg) => {
      let fd = interp(f, env);
      interp(fdCbody(fd), extendEnv(mtEnv, Bind(fdCarg(fd), interp(arg, env))));
    };
    | PlusC(l, r) => numPlus(interp(l, env), interp(r, env));
    | MultC(l, r) => numMult(interp(l, env), interp(r, env));
    // | IfCondC(p, c, a) => if (interp(p, env) == 0) { interp(a, env) } else { interp(c, env) };
  }
}

type rec arithS = 
| NumS(int)
| PlusS(arithS, arithS)
| MultS(arithS, arithS)
| MinusS(arithS, arithS)
| SquareS(arithS)
// | IfCondS(arithS, arithS, arithS)


let rec desugar = (ars) => {
  switch ars {
    | NumS(n) => NumC(n);
    | PlusS(l, r) => PlusC(desugar(l), desugar(r));
    | MultS(l, r) => MultC(desugar(l), desugar(r));
    | MinusS(l, r) => PlusC(desugar(l), MultC(NumC(-1), desugar(r)));
    | SquareS(l) => PlusC(desugar(l), desugar(l));
    // | IfCondS(p, c, a) => IfCondC(desugar(p), desugar(c), desugar(a))
  }
}
let fd1 = FdC("double", "x", PlusC(IdC("x"), NumC(5)));
let fd2 = FdC("quad", "x", AppC(FdC("double", "x", PlusC(IdC("x"), NumC(5))), NumC(5)));
let fd3 = FdC("const5", "_", NumC(5));

let an = interp(PlusC(MultC(AppC(FdC("double", "x", PlusC(IdC("x"), NumC(5))), NumC(2)), NumC(4)), NumC(3)), mtEnv);

Js.log(Value.getNum(an));
// 31

let an1 = interp(desugar(PlusS(SquareS(NumS(8)), NumS(3))), mtEnv);

