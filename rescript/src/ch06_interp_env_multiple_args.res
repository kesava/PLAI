type rec exprC =
  | NumC(int)
  | IdC(string)
  | AppC(string, list<exprC>)
  | PlusC(exprC, exprC)
  | MultC(exprC, exprC)
  | IfCondC(exprC, exprC, exprC)

type name = string
type param = string
type params = list<param>
type args = list<exprC>
type body = exprC
type funDefC = 
  | FdC(name, params, body)

let rec getFunDef = (f: name, fds: list<funDefC>) => {
  switch fds {
  | list{} => raise(Not_found)
  | list{a, ...rest} => {
    switch a {
      | FdC(fname, _, _) => if (f === fname) { a } else { getFunDef(f, rest) };
    }
    }
  }
}

let fdCparams = fd => {
  switch fd {
  | FdC(_, params, _) => params
  }
}

let fdCbody = fd => {
  switch fd {
  | FdC(_, _, b) => b
  }
}
type val = int
type binding =
  | Bind(name, val)

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

let rec extendEnvParamsArgs = (~params: params, ~args: args, ~env: env, ~fds: list<funDefC>) : env => {
  switch args {
  | list{} => env
  | list{arg, ...rest} => extendEnv(extendEnvParamsArgs(~params=Belt.List.tailExn(params), ~args=rest, ~env=env, ~fds=fds), Bind(Belt.List.headExn(params), interp(arg, env, fds)));
  }
}
and interp = (exp, env, fds) => {
  switch exp {
    | NumC(n) => n;
    | IdC(id) => lookup(id, env);
    | AppC(f, args) => {
      let fd = getFunDef(f, fds);
      interp(fdCbody(fd), extendEnvParamsArgs(~params=fdCparams(fd), ~args=args, ~env=env, ~fds=fds), fds);
    };
    | PlusC(l, r) => interp(l, env, fds) + interp(r, env, fds);
    | MultC(l, r) => interp(l, env, fds) * interp(r, env, fds);
    | IfCondC(p, c, a) => if (interp(p, env, fds) == 0) { interp(a, env, fds) } else { interp(c, env, fds) };
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
let fd1 = FdC("double", list{"x"}, PlusC(IdC("x"), NumC(5)));
let fd2 = FdC("quad", list{"x"}, AppC("double", list{AppC("double", list{IdC("x")})}));
let fd3 = FdC("const5", list{"_"}, NumC(5));
let fd4 = FdC("add", list{"x", "y"}, PlusC(IdC("x"), IdC("y")));

let an = interp(PlusC(MultC(AppC("double", list{NumC(2)}), NumC(4)), NumC(3)), mtEnv, list{fd1, fd2, fd3});

Js.log(an);
// 31

let an1 = interp(PlusC(MultC(AppC("add", list{NumC(2), NumC(8)}), NumC(4)), NumC(3)), mtEnv, list{fd1, fd2, fd3, fd4});
Js.log(an);
// 43

let an2 = interp(desugar(PlusS(SquareS(NumS(8)), NumS(3))), mtEnv, list{fd1, fd2, fd3});

