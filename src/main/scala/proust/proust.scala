package proust

type Name = String

enum Exp {
  case Lam(v: Var, e: Exp)
  case App(v: Exp, e: Exp)
  case Ann(e: Exp, t: Typ)
  case Var(n: Name)  
}

enum Typ {
  case Arr(a: Typ, b: Typ)
  case Den(n: Name)
}
