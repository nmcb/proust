package proust

import disjoining._
import option._

// expressions

type Name = String

sealed trait Exp
sealed trait Sym extends Exp {
  def n: Name
}
case class Lam(s: Sym, e: Exp) extends Exp
case class App(f: Exp, x: Exp) extends Exp
case class Ann(e: Exp, t: Typ) extends Exp
case class Var(n: Name)        extends Sym
case class Hol(n: Name = "")   extends Sym {

  def isEmpty: Boolean =
    n == ""

  def nr: Int =
    if (!isEmpty) n.toInt else sys.error("nr on empty hole")
}

object Hol {

  def apply(i: Int): Hol =
    Hol(i.toString)
}

// types

trait Typ 
case class Arr(a: Typ, b: Typ) extends Typ
case class Den(n: Name)        extends Typ

// context

type Ctx = Map[Sym,Typ]

object Ctx {

  def empty: Ctx =
    Map.empty
}

type Holes = Map[Int,(Typ,Ctx)]

object Holes {

  def empty: Holes =
    Map.empty

  def init(nr: Int, typ: Typ): Holes =
    Map(nr -> (typ -> Map.empty))
}

case class Goal( current  : Exp
               , holes    : Holes
               , nextNr   : Int
               , isSolved : Boolean
               )

object Goal {

    import parser._
    import Holes._
    import Opt._

    def apply(task: String): Goal = {
      
      val hypothesis: Typ =
        tparse(task)

      val proof: Exp =
        Ann(Hol(0), hypothesis)
      
      Goal( current  = proof
          , holes    = init(0, hypothesis)
          , nextNr   = 1
          , isSolved = false
          )
    }
}

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

  def variable[V >: Var]: P[V] =
    token(name).map(n => Var(n))

  def hole: P[Hol] =
    for { _ <- reserved("?") ; i <- digit.zeroOrMore } yield Hol(i.mkString)
    
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
      s <- variable
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
    lambda |!| application |!| hole |!| variable |!| annotation |!| apprep

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
        case _                 => sys.error("boom!")
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

  def ppexp(e: Exp): String =
    e match {
      case Lam(s,e) => s"(λ ${ppexp(s)} => ${ppexp(e)})"
      case App(f,a) => s"(${ppexp(f)} ${ppexp(a)})"
      case Hol(n)   => s"?$n"
      case Var(n)   => n
      case Ann(e,t) => s"(${ppexp(e)} : ${pptyp(t)})"
    }

  def pptyp(t: Typ): String =
    t match {
      case Arr(a,b) => s"(${pptyp(a)} -> ${pptyp(b)})"
      case Den(n)   => n
    }

  def ppctx(c: Ctx): String = 
    c.map((s,t) => s"\n${s.n} : ${pptyp(t)}").mkString

  def pptask(t: Exp): String =
    s"${printer.ppexp(t)}\n"
  
  def ppholes(holes: Holes): String =
    holes
      .map({case (n,(t,c)) => s"Goal $n has type ${pptyp(t)} in context ${ppctx(c)}"})
      .mkString
  
  def ppgoal(g: Goal): String =
    s"""Goal with ${g.holes.keys.size} hole${if (g.holes.keys.size > 1) "s" else ""} is now
        |${ppexp(g.current)}
        |${ppholes(g.holes)}
      """.stripMargin
}

object typer {

  import printer._
  import Opt._

  def check(ctx: Ctx, exp: Exp, typ: Typ, ref: Boolean = false): (Boolean,Ctx) = {

    def cerror(msg: String = "Unable to check.") = 
      sys.error(cinfo(msg))

    def cinfo(msg: String = ""): String =
      s"""CHECK ${if (ref) then "[refining] " + msg else msg}
         |Exp: ${ppexp(exp)}
         |Typ: ${pptyp(typ)}
         |Ctx: ${ppctx(ctx)}
      """.stripMargin

    (exp,typ) match {
      case ( Lam(s,e) , Arr(a,b) ) => check(ctx + (s -> a), e, b, ref)
      case ( Lam(x,t) , _        ) => cerror()
      case ( Hol(n)   , _        ) => if (ref)
                                        then (true, ctx + (Hol(n) -> typ))
                                        else (true, ctx)
      case _                       => if (typ == synth(ctx, exp, ref))
                                        then (true, ctx)
                                        else cerror()
    }
  }

  def synth(ctx: Ctx, exp: Exp, ref: Boolean = false): Typ = {

    def serror(msg: String = "unable to synth") = 
      sys.error(sinfo(msg))

    def sinfo(msg: String = ""): String =
      s"""synth ${if (ref) then s"[refining] - $msg" else msg}
         |exp: ${ppexp(exp)}
         |ctx: ${ppctx(ctx)}
       """.stripMargin

    exp match {
      case Lam(_,_)                                 => serror()
      case Hol(n)                                   => serror()
      case Ann(e,t) if check(ctx, e, t, ref)._1     => t
      case App(f,x)                                 =>
        synth(ctx, f) match {
          case Arr(a,b) if check(ctx, x, a, ref)._1 => b
          case _                                    => serror()
        }
      case Var(n)                                   => ctx.getOrElse(Var(n), serror())
      case _                                        => serror()
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
  import printer._
  import State._
  import Opt._

  def number(exp: Exp, ctr: Int): (Exp, Int) =
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
      case Var(n) =>
        (Var(n), ctr)
      case Ann(e,t) => {
        val (ne,nc) = number(e, ctr) 
        (Ann(ne, t), nc)
      }
      case Hol("") => (Hol(ctr), ctr + 1)
      case Hol(n)  => (Hol(n),  ctr + 1)
    }

  def replace(nr: Int, rep: Exp, exp: Exp): Exp =
    exp match {
      case Lam(s,e)    => Lam(s, replace(nr, rep, e))
      case App(f,a)    => App(replace(nr, rep, f), replace(nr, rep, a))
      case Var(n)      => Var(n)
      case Ann(e,t)    => Ann(replace(nr, rep, e), t)
      case Hol("")     => sys.error(s"unnumbered hole in expression: $exp")
      case Hol(n)      => if (n.toInt == nr) then rep else Hol(n)
    }

  def refine(hole: Int, exp: String): State[Goal,Goal] = {

    val refinement: Exp =
      eparse(exp)
      
    State(goal => {

      val (t,c)    = goal.holes.getOrElse(hole, sys.error(s"no hole data $hole"))
      val _        = check(c, refinement, t)
      val (ne, nc) = number(refinement, goal.nextNr)
      val (_, ctx) = check(c, ne, t, ref = true)

      val ngoal = if (nc != hole+1) {
        val nhole    = (hole+1) -> (ctx(Hol(hole+1)) ->  ctx)
        val nexp     = replace(hole, ne, goal.current)
        val nholes   = goal.holes - hole + nhole
        goal.copy(nexp, nholes, nc)
      } else {
        goal.copy(ne, Holes.empty, -1, true)
      }

      (ngoal,ngoal)
    })
  }
}
