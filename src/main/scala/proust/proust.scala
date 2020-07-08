package proust

type Name = String

sealed trait Exp
case class Lam(s: Sym, e: Exp) extends Exp
case class App(f: Exp, a: Exp) extends Exp
case class Ann(e: Exp, t: Typ) extends Exp
case class Sym(n: Name)        extends Exp

trait Typ 
case class Arr(a: Typ, b: Typ) extends Typ
case class Den(n: Name)        extends Typ

type Ctx = Map[Sym,Typ]

object parser {

  import disjoining._
  import parsing._
  import P._

  def name: P[Name] =
    oneOf("abcdefghifklmnopqrstuvwxyz-").oneOrMore.map(_.mkString)

  def typeName: P[Name] =
    oneOf("ABCDEFGHIJKLMNOPQRSTUVWXYZ-").oneOrMore.map(_.mkString)

  def symbol[S >: Sym]: P[S] =
    token(name).map(n => Sym(n))

  def application: P[Exp] =
    for { 
      _  <- reserved("(") 
      e1 <- expression
      e2 <- expression
      _  <- reserved(")")
    } yield App(e1, e2)

  def lambda: P[Exp] =
    for {
      _ <- reserved("(λ")
      s <- symbol
      _ <- reserved("=>")
      e <- expression
      _ <- reserved(")")
    } yield Lam(s, e)

  def annotation: P[Exp] =
    for {
      _ <- reserved("(")
      e <- expression
      _ <- reserved(":")
      t <- typ
      _ <- reserved(")")
    } yield Ann(e, t)

  def apprep: P[Exp] = {
    for {
      _  <- reserved("(")
      e1 <- expression
      e2 <- expression
      es  <- expression.oneOrMore
      _  <- reserved(")")
    } yield es.foldLeft(App(e1,e2))((a,e) => App(a, e))
  }

  def expression: P[Exp] =
    lambda |!| application |!| symbol |!| annotation |!| apprep

  def denotation: P[Den] =
    token(typeName).map(n => Den(n))
  
  def arrow: P[Typ] =
    for {
      _ <- reserved("(")
      f <- typ
      _ <- reserved("->")
      t <- typ
      _ <- reserved(")")
    } yield Arr(f, t)

  def arrrep: P[Typ] = {

    def rightAssoc(ts: List[Typ]): Typ =
      ts.reverse match {
        case a1 :: a2 :: tail => tail.foldLeft(Arr(a2, a1))((a,e) => Arr(e,a))
        case _                => sys.error("boom!")
      }

    for { 
      _  <- reserved("(")
      t1 <- typ
      _  <- reserved("->")
      t2 <- typ
      _  <- reserved("->")
      r  <- seperated("->", typ)
      _  <- reserved(")")
    } yield rightAssoc(t1 :: t2 :: r)
  }

  def typ: P[Typ] =
    arrow |!| arrrep |!| denotation

  def parse(s: String): Exp =
    run(expression)(s)
}

object printer {

  def print(e: Exp): String =
    e match {
      case Lam(s,e) => s"(λ ${print(s)} => ${print(e)})"
      case App(f,a) => s"(${print(f)} ${print(a)})"
      case Sym(n)   => n
      case Ann(e,t) => s"(${print(e)} : ${print(t)})"
    }

  def print(t: Typ): String =
    t match {
      case Arr(a,b) => s"(${print(a)} -> ${print(b)})"
      case Den(n)   => n
    }

  def print(c: Ctx): String =
    c.map((s,t) => s"\n$s : ${print(t)}").mkString
}

object typer {

  def check(c: Ctx, e: Exp, t: Typ): Boolean = {

    def cerror =
      sys.error(
        s""" Unable to check
           |
           | Exp: ${print(e)}
           | Typ: ${print(t)}
           | Ctx: ${print(c)}
        """.stripMargin)

    (e,t) match {
      case (Lam(s,e),Arr(a,b))  => check(c + (s -> a), e, b)
      case (Lam(x,t),_)         => cerror
      case _ if t == synth(c,e) => true
      case _                    => cerror
    }
  }

  def synth(ctx: Ctx, exp: Exp): Typ = {

    def serror =
      sys.error(
        s""" Unable to synth
            |
            | Exp: ${print(exp)}
            | Ctx: ${print(ctx)}
        """.stripMargin)
    
    exp match {
      case Lam(_,_)                         => serror
      case Ann(e,t) if check(ctx, e, t)     => t
      case App(f,x)                         =>
        synth(ctx,f) match {
          case Arr(a,b) if check(ctx, x, a) => b
          case _                            => serror
        }
      case s: Sym                           => ctx.getOrElse(s, serror)
      case _                                => serror
    }
  }
}