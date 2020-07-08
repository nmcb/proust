package proust

type Name = String

sealed trait Exp
case class Lam(s: Sym, e: Exp) extends Exp
case class App(f: Exp, x: Exp) extends Exp
case class Ann(e: Exp, t: Typ) extends Exp
case class Sym(n: Name)        extends Exp

trait Typ 
case class Arr(a: Typ, b: Typ) extends Typ
case class Den(n: Name)        extends Typ

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

  def parseExpr(s: String): Exp =
    run(expression)(s)
}
