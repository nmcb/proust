package proust

type Name = String

trait Exp
case class Lam(v: Var, e: Exp) extends Exp
case class App(f: Exp, x: Exp) extends Exp
case class Ann(e: Exp, t: Typ) extends Exp
case class Var(n: Name)        extends Exp

trait Typ 
case class Arr(a: Typ, b: Typ) extends Typ
case class Den(n: Name)        extends Typ

object parser {

  import P._

  def name: P[Name] =
    oneOf("abcdefghifklmnopqrstuvwxyz-").oneOrMore.map(_.mkString)

  def typeName: P[Name] =
    oneOf("ABCDEFGHIJKLMNOPQRSTUVWXYZ-").oneOrMore.map(_.mkString)

  def variable: P[Var] =
    token(name).map(n => Var(n))

  def application: P[Exp] =
    for { 
      _ <- reserved("(")
      f <- expression
      x <- expression
      _ <- reserved(")")
    } yield App(f, x)

  def lambda: P[Exp] =
    for {
      _ <- reserved("(λ")
      v <- variable
      _ <- reserved("=>")
      e <- expression
      _ <- reserved(")")
    } yield Lam(v, e)

  def annotation: P[Exp] =
    for {
      _ <- reserved("(")
      e <- expression
      _ <- reserved(":")
      t <- typ
      _ <- reserved(")")
    } yield Ann(e, t)

  def expression: P[Exp] =
    lambda |!| application |!| variable |!| annotation

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

  def typ: P[Typ] =
    arrow |!| denotation

  def parseExpr(s: String): Exp =
    run(expression)(s)
  
}
