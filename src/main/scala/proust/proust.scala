package proust

import O._

type Name = String

sealed trait E

sealed trait Exp
case class Lam(s: Sym, e: Exp) extends Exp
case class App(f: Exp, x: Exp) extends Exp
case class Ann(e: Exp, t: Typ) extends Exp
case class Sym(n: Name)        extends Exp

trait Typ 
case class Arr(a: Typ, b: Typ) extends Typ
case class Den(n: Name)        extends Typ

object parser {

  import P._

  def name: P[Name] =
    oneOf("abcdefghifklmnopqrstuvwxyz-").oneOrMore.map(_.mkString)

  def typeName: P[Name] =
    oneOf("ABCDEFGHIJKLMNOPQRSTUVWXYZ-").oneOrMore.map(_.mkString)

  def symbol[S >: Sym]: P[S] =
    token(name).map(n => Sym(n))

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

  def expression: P[Exp] =
    lambda |!| application |!| symbol |!| annotation

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
