package proust

import disjoining._
import option._

type Name = String

sealed trait Exp
case class Lam(s: Sym, e: Exp) extends Exp
case class App(f: Exp, a: Exp) extends Exp
case class Ann(e: Exp, t: Typ) extends Exp
case class Sym(n: Name)        extends Exp
case class Hol(c: Opt[Int])    extends Exp

trait Typ 
case class Arr(a: Typ, b: Typ) extends Typ
case class Den(n: Name)        extends Typ

type Ctx = Map[String,Typ]

object parser {

  import sequencing._
  import Seq._
  import parsing._
  import calculator._
  import P._

  def name: P[Name] =
    oneOf("abcdefghifklmnopqrstuvwxyz-").oneOrMore.map(_.mkString)

  def typeName: P[Name] =
    oneOf("ABCDEFGHIJKLMNOPQRSTUVWXYZ-").oneOrMore.map(_.mkString)

  def symbol[S >: Sym]: P[S] =
    token(name).map(n => Sym(n))

  def hole: P[Hol] =
    for {
      _ <- reserved("?")
      i <- number.zeroOrMore
    } yield Hol(i.opt)
    
  
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
      es <- expression.oneOrMore
      _  <- reserved(")")
    } yield es.foldl(App(e1,e2))(a => e => App(a, e))
  }

  def expression: P[Exp] =
    lambda |!| application |!| hole |!| symbol |!| annotation |!| apprep

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

    def rassoc(ts: Seq[Typ]): Typ =
      ts.reverse match {
        case Cel(a1,Cel(a2,r)) => r.foldl(Arr(a2, a1))(a => e => Arr(e,a))
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
    } yield rassoc(t1 :: t2 :: r)
  }

  def typ: P[Typ] =
    arrow |!| arrrep |!| denotation

  def eparse(s: String): Exp =
    run(expression)(s)

  def tparse(s: String): Typ =
    run(typ)(s)
}

object printer {

  def pprint(e: Exp): String =
    e match {
      case Lam(s,e) => s"(λ ${pprint(s)} => ${pprint(e)})"
      case App(f,a) => s"(${pprint(f)} ${pprint(a)})"
      case Hol(i)   => s"?$i"
      case Sym(n)   => n
      case Ann(e,t) => s"(${pprint(e)} : ${pprint(t)})"
    }

  def pprint(t: Typ): String =
    t match {
      case Arr(a,b) => s"(${pprint(a)} -> ${pprint(b)})"
      case Den(n)   => n
    }

  def pprint(c: Ctx): String =
    c.map((s,t) => s"\n$s : ${pprint(t)}").mkString
}

object typer {

  import printer._

  def check(ctx: Ctx, exp: Exp, typ: Typ): Boolean = {

    def cerror =
      sys.error(
        s""" Unable to check.
           | Exp: ${pprint(exp)}
           | Typ: ${pprint(typ)}
           | Ctx: ${pprint(ctx)}
        """.stripMargin)

    (exp,typ) match {
      case (Lam(s,e),Arr(a,b))         => check(ctx + (s.n -> a), e, b)
      case (Lam(x,t),_)                => cerror
      case _ if typ == synth(ctx, exp) => true
      case _                           => cerror
    }
  }

  def synth(ctx: Ctx, exp: Exp): Typ = {

    def serror =
      sys.error(
        s""" Unable to synth.
           | Exp: ${pprint(exp)}
           | Ctx: ${pprint(ctx)}
        """.stripMargin)
    
    exp match {
      case Lam(_,_)                         => serror
      case Ann(e,t) if check(ctx, e, t)     => t
      case App(f,x)                         =>
        synth(ctx, f) match {
          case Arr(a,b) if check(ctx, x, a) => b
          case _                            => serror
        }
      case Sym(n)                           => ctx.getOrElse(n, serror)
      case _                                => serror
    }
  }
}

case class State[S,A](run: S => (A,S)) {

  def get: State[S,S] =
    State(s => (s,s))

  def set(s: S): State[S,Unit] =
    State(_ => ((),s))

  def modify(f: S => S): State[S,Unit] =
    for { s <- get ; _ <- set(f(s)) } yield ()

  def map[B](f: A => B): State[S,B] =
    flatMap(a => State(s => (f(a),s)))

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(s => { val (a,ss) = run(s) ; f(a).run(ss) })

  def withFilter(p: A => Boolean): scala.collection.WithFilter[A, scala.Seq] =
    ???    
}

object State {

  def unit[S,A](a: A): State[S,A] =
    State[S,A](s => (a,s))

}

object assistent {

  import parser._
  import typer._
  import State._
  import Opt._

  case class Goal(current: Exp, holes: Map[Int,Typ] = Map.empty, counter: Int = 0)

  def task(s: String): State[Goal, Unit] = {
    val n    = 0
    val typ  = tparse(s)
    val exp  = Ann(Hol(The(n)),typ)
    val goal = Goal(exp)

    State(_ => ((), goal.copy(holes = Map.empty + (n -> typ), counter = n)))
  }

  private def number(exp: Exp, ctr: Int): (Exp, Int) =
    exp match {
      case Lam(s,e) => {
        val (ne,nc) = number(e, ctr)
        (Lam(s,ne),nc)
      }
      case App(f,a) => {
        val (e1,c1) = number(f, ctr)
        val (e2,c2) = number(a, c1)
        (App(e1, e2), c2)
      }
      case Sym(n)    => (Sym(n), ctr)
      case Ann(e,t)  => {
        val (ne,nc) = number(e, ctr) 
        (Ann(ne, t), nc)
      }
      case Hol(en)    => (Hol(Opt(en.getOrElse(ctr + 1))), ctr + 1)
    }
  
  def refine(n: Int, exp: String)(s: State[Goal,Unit]): State[Goal,Unit] =
    for {
      goal     <- s.get
      typ      <- State.unit(goal.holes.getOrElse(n, sys.error(s"No goal: $n")))
      e         = eparse(exp)
      (ne, nc)  = number(e, goal.counter)
      ng        = goal.copy(current = ne, holes = (goal.holes - nc) , nc)
      _        <- s.set(ng)
    } yield ()
} 
