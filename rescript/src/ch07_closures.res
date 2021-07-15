
  type name = string
  type arg = string
  
  type rec exprC =
  | NumC(int)
  | IdC(string)
  | LamC(arg, exprC)
  | AppC(exprC, exprC)
  | PlusC(exprC, exprC)
  | MultC(exprC, exprC)
  // | IfCondC(exprC, exprC, exprC)

module Types = {
  
  type rec binding =
    | Bind(name, value)
  and value =
  | NumV(int)
  | ClosV(arg, exprC, env)
  and env = list<binding>

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

  let closVArg = (x) => 
    switch x {
      | ClosV(a, _, _) => a
      | _ => raise(Not_found);
    }

  let closVBody = (x) => {
    switch x {
      | ClosV(_, b, _) => b;
      | _ => raise(Not_found);
    }
  }

  let closVEnv = (x) => {
    switch x {
      | ClosV(_, _, e) => e;
      | _ => raise(Not_found);
    }
  }
};

let mtEnv: Types.env = list{}
let extendEnv = Belt.List.add

let rec lookup = (n, env) => {
  switch env {
  | list{} => raise(Not_found)
  | list{a, ...rest} => {
    switch a {
    | Types.Bind(k, v) => if (k === n) { v } else { lookup(n, rest)}
    }
    }
  }
}

let numPlus = (l: Types.value, r: Types.value) : Types.value => {
  if (Types.isNum(l) && Types.isNum(r)) {
    Types.NumV(Types.getNum(l) + Types.getNum(r));
  } else {
    raise(Not_found);
  }
}

let numMult = (l: Types.value, r: Types.value) : Types.value => {
  if (Types.isNum(l) && Types.isNum(r)) {
    Types.NumV(Types.getNum(l) * Types.getNum(r));
  } else {
    raise(Not_found);
  }
}

let rec interp = (exp, env): Types.value => {
  switch exp {
    | NumC(n) => NumV(n);
    | IdC(id) => lookup(id, env);
    | LamC(arg, body) => Types.ClosV(arg, body, env);
    | AppC(f, arg) => {
      let fValue = interp(f, env); // must be a ClosV
      interp(Types.closVBody(fValue), extendEnv(Types.closVEnv(fValue), Bind(Types.closVArg(fValue), interp(arg, env))));
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
let fd1 = LamC("x", PlusC(IdC("x"), NumC(5)));
let fd2 = LamC("y", AppC(LamC("x", PlusC(IdC("x"), NumC(5))), NumC(5)));
let fd3 = LamC("_", NumC(5));

let an = interp(PlusC(MultC(AppC(fd1, NumC(2)), NumC(4)), NumC(3)), mtEnv);
Js.log(Types.getNum(an));
// 31

let an1 = interp(desugar(PlusS(SquareS(NumS(8)), NumS(3))), mtEnv);

