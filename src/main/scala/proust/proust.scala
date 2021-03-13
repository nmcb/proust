package proust

import sequencing._

// expressions

type Name = String
type Err  = Exception

sealed trait Exp
sealed trait Sym extends Exp {
  def n: Name
}
case class Lam(s: Sym, e: Exp)         extends Exp
case class App(f: Exp, x: Exp)         extends Exp
case class Ann(e: Exp, t: Typ)         extends Exp
case class Prd(l: Exp, r: Exp)         extends Exp
case class Lhs(e: Exp)                 extends Exp
case class Rhs(e: Exp)                 extends Exp
case class Sum(i: Exp, l: Exp, r: Exp) extends Exp
case class Var(n: Name)                extends Sym
case class Hol(n: Name = "")           extends Sym {

  def isEmpty: Boolean =
    n == ""

  def nr: Int =
    if (!isEmpty) n.toInt else sys.error("nr on empty hole")
}

object Hol {

  def apply(i: Int): Hol =
    Hol(i.toString)

  val empty: Hol =
    Hol("")
}

// types

trait Typ 
case class Arr(a: Typ, b: Typ) extends Typ
case class Den(n: Name)        extends Typ
case class PTp(l: Typ, r: Typ) extends Typ
case class STp(l: Typ, r: Typ) extends Typ

// context

type Ctx = Map[Sym,Typ]

object Ctx {

  def empty: Ctx =
    Map.empty
}

type Hole = (Int,Typ,Ctx)

case class Holes(holes : Map[Int,(Typ,Ctx)] = Map[Int,(Typ,Ctx)]()) {

  def get(hole: Int): (Typ,Ctx) =
    holes.getOrElse(hole, sys.error(s"hole $hole doesn't exist"))
  
  def numbers: Seq[Int] =
    Seq(holes.keys.toList)

  def -(hole: Int): Holes =
    Holes(holes - hole)

  def +(hole: Hole): Holes =
    Holes(holes + (hole._1 -> (hole._2 -> hole._3)))

  def map[B](f: Hole => B): Seq[B] =
    Seq(holes.map({ case (n,(t,c)) => f((n,t,c))}))
}

object Holes {

  def empty: Holes =
    Holes()

  def apply(typ: Typ): Holes =
    Holes(Map(0 -> (typ -> Map.empty)))
}

case class Goal( current  : Exp
               , holes    : Holes
               , nextNr   : Int
               , isSolved : Boolean
               , trace    : Boolean
               )

object Goal {

    import disjoining._
    import parser._
    import Holes._

    def apply(task: String, trace: Boolean = false): Goal = {
      
      val hypothesis: Typ =
        tparse(task)

      val proof: Exp =
        Ann(Hol(0), hypothesis)
      
      Goal( current  = proof
          , holes    = Holes(hypothesis)
          , nextNr   = 1
          , isSolved = false
          , trace    = trace
          )
    }
}

object parser {

  import sequencing._
  import Seq._
  import parsing._
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
      f  <- expression
      x  <- expression
      _  <- reserved(")")
    } yield App(f, x)

  def lambda: P[Exp] =
    for {
      _  <- reserved("(λ")
      s  <- variable
      _  <- reserved("=>")
      e  <- expression
      _  <- reserved(")")
    } yield Lam(s, e)

  def annotation: P[Exp] =
    for {
      _  <- reserved("(")
      e  <- expression
      _  <- reserved(":")
      t  <- typ
      _  <- reserved(")")
    } yield Ann(e, t)
  
  def product: P[Exp] =
    for {
      _  <- reserved("(and")
      l  <- expression
      r  <- expression
      _  <- reserved(")")
    } yield Prd(l, r)

  def lhs: P[Exp] =
    for {
      _  <- reserved("(lhs")
      e  <- expression
      _  <- reserved(")")
    } yield Lhs(e)
      
  def rhs: P[Exp] =
    for {
      _  <- reserved("(rhs")
      e  <- expression
      _  <- reserved(")")
    } yield Rhs(e)
    
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
    product |!| lhs |!| rhs |!| lambda |!| hole |!| variable |!| annotation |!| application |!| apprep

  def denotation: P[Den] =
    token(typeName).map(n => Den(n))

  def andtyp: P[Typ] =
    for {
      _ <- reserved("(")
      a <- typ
      _ <- reserved("∧")
      b <- typ
      _ <- reserved(")")
    } yield PTp(a, b)
    
  def ortyp: P[Typ] =
    for {
      _ <- reserved("(")
      a <- typ
      _ <- reserved("∨")
      b <- typ
      _ <- reserved(")")
    } yield STp(a, b)
  
  def arrow: P[Typ] =
    for {
      _ <- reserved("(")
      a <- typ
      _ <- reserved("->")
      b <- typ
      _ <- reserved(")")
    } yield Arr(a, b)

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
      r  <- separated("->", typ)
      _  <- reserved(")")
    } yield rassoc(t1 :: t2 :: r)
  }

  def typ: P[Typ] =
    arrow |!| arrrep |!| andtyp |!| ortyp |!| denotation

  def eparse(s: String): Exp =
    run(expression)(s)

  def tparse(s: String): Typ =
    run(typ)(s)
}

object printer {

  def ppexp(e: Exp): String =
    e match {
      case Lam(s,e)   => s"(λ ${ppexp(s)} => ${ppexp(e)})"
      case App(f,a)   => s"(${ppexp(f)} ${ppexp(a)})"
      case Hol(n)     => s"?$n"
      case Var(n)     => n
      case Ann(e,t)   => s"(${ppexp(e)} : ${pptyp(t)})"
      case Prd(e1,e2) => s"(and ${ppexp(e1)} ${ppexp(e2)})"
      case Lhs(e)     => s"(lhs ${ppexp(e)})"
      case Rhs(e)     => s"(rhs ${ppexp(e)})"
      case Sum(i,l,r) => s"(or ${ppexp(i)} ${ppexp(l)} ${ppexp(r)})"
    }

  def pptyp(t: Typ): String =
    t match {
      case Den(n)   => n
      case PTp(a,b) => s"${pptyp(a)} ∧ ${pptyp(b)}"
      case STp(a,b) => s"${pptyp(a)} ∨ ${pptyp(b)}"
      case Arr(a,b) => s"(${pptyp(a)} -> ${pptyp(b)})"
    }

  def ppctx(c: Ctx): String = 
    c.map((s,t) => s"\n${s.n} : ${pptyp(t)}").mkString

  def ppholes(holes: Holes): String =
    holes
      .map((nr,t,c) => s"[$nr] : ${pptyp(t)} in context ${ppctx(c)}")
      .mkString
  
  def ppgoal(g: Goal): String =
    s"""${g.holes.numbers.size} Goals in ${ppexp(g.current)}
        |${ppholes(g.holes)}
        |${if (g.isSolved) "\nSolved." else ""}
      """.stripMargin

  def pprint: State[Goal,Unit] =
    State(g => (println(ppgoal(g)),g))
}

object typer {

  import printer._

  def check(ctx: Ctx, exp: Exp, typ: Typ, ref: Boolean = false): (Boolean,Ctx) = {

    def cerror(msg: String = "unable to check") = 
      sys.error(cinfo(msg))

    def cinfo(msg: String = ""): String =
      s"""CHECK ${if (ref) then "[refining] " + msg else msg}
         |Exp: ${ppexp(exp)}
         |Typ: ${pptyp(typ)}
         |Ctx: ${ppctx(ctx)}
      """.stripMargin.trim

    println(cinfo())

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

    def sinfo(msg: String = "trace"): String =
      s"""SYNTH ${if (ref) then s"[refining] - $msg" else msg}
         |exp: ${ppexp(exp)}
         |ctx: ${ppctx(ctx)}
       """.stripMargin

    println(sinfo())

    exp match {
      case Lam(_,_)                                 => serror()
      case Hol(_)                                   => serror()
      case Ann(e,t) if check(ctx, e, t, ref)._1     => t
      case Prd(a,b)                                 => PTp(synth(ctx, a, ref), synth(ctx, b, ref))
      case App(f,x)                                 =>
        synth(ctx, f, ref) match {
          case Arr(a,b) if check(ctx, x, a, ref)._1 => b
          case _                                    => serror()
        }
      case Var(n)                                   => ctx.getOrElse(Var(n), serror())
      case Lhs(e)                                   =>
        synth(ctx, e, ref) match {
          case PTp(l,_)                             => l
          case _                                    => serror()
        }
      case Rhs(e)                                   =>
        synth(ctx, e, ref) match {
          case PTp(_,r)                             => r
          case _                                    => serror()
        }
      case _                                        => serror()
    }
  }
}

case class State[S,A](run: S => (A,S)) {

  def map[B](f: A => B): State[S,B] =
    flatMap(a => State(s => (f(a),s)))

  def map2[B](fa: S => B, fs: S => S): State[S,B] =
    State(s => (fa(s), fs(s)))

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(s => { val (a,ss) = run(s) ; f(a).run(ss) })

  def withFilter(p: A => Boolean): scala.collection.WithFilter[A, scala.Seq] =
    ???

}

object State {

  def pure[S,A](a: A): State[S,A] =
    State(s => (a,s))

  def inspect[S,A](f: S => A): State[S,A] =
    State(s => (f(s),s))

  def bimap[S,A](fa: S => A, fs: S => S): State[S,A] =
    State(s => (fa(s),fs(s)))
  
  def get[S]: State[S,S] =
    inspect(identity)

  def set[S](s: S): State[S,Unit] =
    State(_ => ((),s))

  def modify[S](f: S => S): State[S,Unit] =
    State(s => ((), f(s)))

}

object assistent {

  import printer._
  import parser._
  import typer._
  import State._

  def number(exp: Exp): State[Int,Exp] = {
    exp match {
      case Lam(s,e)   => number(e).map(ne => Lam(s, ne))
      case App(f,a)   => for { e1 <- number(f) ; e2 <- number(a) } yield App(e1, e2)
      case v: Var     => pure(v)
      case Ann(e,t)   => number(e).map(ne => Ann(ne, t))
      case Hol.empty  => bimap(n => Hol(n), n => n + 1)
      case h: Hol     => pure(h)
      case Prd(l,r)   => for { nl <- number(l) ; nr <- number(r) } yield Prd(nl ,nr)
      case Lhs(e)     => number(e).map(ne => Lhs(ne))
      case Rhs(e)     => number(e).map(ne => Rhs(ne))
      case Sum(i,l,r) => for {ni <- number(i); nl <- number(l); nr <- number(r)} yield Sum(ni, nl, nr)
    }
  }

  def fpnumber(exp: Exp, ctr: Int): (Exp, Int) =
    (for { ne <- number(exp) } yield ne).run(ctr)

  def replace(nr: Int, rep: Exp, exp: Exp): Exp =
    exp match {
      case Lam(s,e)   => Lam(s, replace(nr, rep, e))
      case App(f,a)   => App(replace(nr, rep, f), replace(nr, rep, a))
      case Var(n)     => Var(n)
      case Ann(e,t)   => Ann(replace(nr, rep, e), t)
      case Hol("")    => sys.error(s"unnumbered hole in expression: $exp")
      case Hol(n)     => if (n.toInt == nr) then rep else Hol(n)
      case Prd(e1,e2) => Prd(replace(nr, rep, e1), replace(nr, rep, e2))
      case Lhs(e)     => Lhs(replace(nr, rep, e))
      case Rhs(e)     => Rhs(replace(nr, rep, e))
      case Sum(i,l,r) => Sum(replace(nr, rep, i), replace(nr, rep, l), replace(nr, rep, r))
    }

  def task(task: String): State[Goal,Exp] =
    State(_ => {
      val goal = Goal(task)
      (goal.current , goal)
    })

  def solved: State[Goal,Boolean] =
    State(g => (g.isSolved, g))

  def refine(hole: Int, exp: String): State[Goal,Goal] = {

    val refinement: Exp =
      eparse(exp)

      
    State(goal => {

      if (goal.trace) println(ppgoal(goal))

      val (t,c)    = goal.holes.get(hole)
      val _        = check(c, refinement, t)
      val (ne, nc) = fpnumber(refinement, goal.nextNr)
      val (_, ctx) = check(c, ne, t, ref = true)

      val ngoal = if (nc != hole+1) {
        val nhole    = ( hole+1 , ctx(Hol(hole+1)) ,  ctx )
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
