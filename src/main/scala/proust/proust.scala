package proust

import sequencing.*

import scala.annotation.tailrec

// expressions

type Name = String
type Err  = Exception

enum Exp:
  case Lam(s: Name, e: Exp)
  case App(f: Exp, x: Exp)
  case Ann(e: Exp, t: Typ)
  case Prd(l: Exp, r: Exp)
  case Lhs(e: Exp)
  case Rhs(e: Exp)
  case Sum(i: Exp, l: Exp, r: Exp)
  case Var(n: Name)
  case Hol(n: Int)

enum Typ:
  case Arr(a: Typ, b: Typ)
  case Den(n: Name)
  case PTp(l: Typ, r: Typ)
  case STp(l: Typ, r: Typ)

import Typ.*
import Exp.*

// context

type Ctx = Map[Name, Typ]

object Ctx:
  def empty: Ctx =
    Map.empty

type Hole = (Int, Typ, Ctx)

case class Holes(holes : Map[Int, (Typ, Ctx)] = Map[Int, (Typ, Ctx)]()):

  def get(hole: Int): (Typ, Ctx) =
    holes.getOrElse(hole, sys.error(s"hole $hole doesn't exist"))
  
  def numbers: Seq[Int] =
    Seq(holes.keys.toList)

  def -(hole: Int): Holes =
    Holes(holes - hole)

  def +(hole: Hole): Holes =
    Holes(holes + (hole._1 -> (hole._2 -> hole._3)))

  def map[B](f: Hole => B): Seq[B] =
    Seq(holes.map({ case (n,(t,c)) => f((n,t,c))}))

private object Holes:

  def empty: Holes =
    Holes()

  def make(typ: Typ): Holes =
    Holes(Map(0 -> (typ -> Map.empty)))

case class Goal( current  : Exp
               , holes    : Holes
               , nextNr   : Int
               , isSolved : Boolean
               , trace    : Boolean
               )

object Goal:

    import parser.*

    def apply(task: String, trace: Boolean = false): Goal =
      val hypothesis: Typ =
        tparse(task)

      val proof: Exp =
        Ann(Hol(0), hypothesis)
      
      Goal( current  = proof
          , holes    = Holes.make(hypothesis)
          , nextNr   = 1
          , isSolved = false
          , trace    = trace
          )

object parser:

  import sequencing.*
  import Seq.*
  import parsing.*
  import P.*

  def name: P[Name] =
    oneOf("abcdefghifklmnopqrstuvwxyz-").oneOrMore.map(_.mkString)

  def typeName: P[Name] =
    oneOf("ABCDEFGHIJKLMNOPQRSTUVWXYZ-").oneOrMore.map(_.mkString)

  def variable[V >: Var]: P[V] =
    token(name).map(n => Var(n))

  def hole: P[Hol] =
    for
      _ <- reserved("?")
      i <- digit.zeroOrMore
    yield
      if i.length == 0 then Hol(-1) else Hol(i.mkString.toInt)
    
  def application: P[Exp] =
    for
      _  <- reserved("(") 
      f  <- expression
      x  <- expression
      _  <- reserved(")")
    yield
      App(f, x)

  def lambda: P[Exp] =
    for
      _  <- reserved("(λ")
      v  <- variable
      _  <- reserved("=>")
      e  <- expression
      _  <- reserved(")")
    yield
      Lam(v.n, e)

  def annotation: P[Exp] =
    for
      _  <- reserved("(")
      e  <- expression
      _  <- reserved(":")
      t  <- typ
      _  <- reserved(")")
    yield
      Ann(e, t)
  
  def product: P[Exp] =
    for
      _  <- reserved("(and")
      l  <- expression
      r  <- expression
      _  <- reserved(")")
    yield
      Prd(l, r)

  def lhs: P[Exp] =
    for
      _  <- reserved("(lhs")
      e  <- expression
      _  <- reserved(")")
    yield
      Lhs(e)
      
  def rhs: P[Exp] =
    for
      _  <- reserved("(rhs")
      e  <- expression
      _  <- reserved(")")
    yield
      Rhs(e)
    
  private def apprep: P[Exp] =
    for
      _  <- reserved("(")
      e1 <- expression
      e2 <- expression
      es <- expression.oneOrMore
      _  <- reserved(")")
    yield
      es.foldLeft(App(e1,e2))(a => e => App(a, e))

  def expression: P[Exp] =
    product |!| lhs |!| rhs |!| lambda |!| hole |!| variable |!| annotation |!| application |!| apprep

  def denotation: P[Den] =
    token(typeName).map(n => Den(n))

  def andtyp: P[Typ] =
    for
      _ <- reserved("(")
      a <- typ
      _ <- reserved("∧")
      b <- typ
      _ <- reserved(")")
    yield
      PTp(a, b)
    
  def ortyp: P[Typ] =
    for
      _ <- reserved("(")
      a <- typ
      _ <- reserved("∨")
      b <- typ
      _ <- reserved(")")
    yield
      STp(a, b)
  
  def arrow: P[Typ] =
    for
      _ <- reserved("(")
      a <- typ
      _ <- reserved("->")
      b <- typ
      _ <- reserved(")")
    yield
      Arr(a, b)

  private def arrrep: P[Typ] =
    def rassoc(ts: Seq[Typ]): Typ =
      ts.reverse match
        case Cel(a1,Cel(a2,r)) => r.foldLeft(Arr(a2, a1))(a => e => Arr(e,a))
        case _                 => sys.error("boom!")

    for
      _  <- reserved("(")
      t1 <- typ
      _  <- reserved("->")
      t2 <- typ
      _  <- reserved("->")
      r  <- separated("->", typ)
      _  <- reserved(")")
    yield
      rassoc(t1 :: t2 :: r)

  def typ: P[Typ] =
    arrow |!| arrrep |!| andtyp |!| ortyp |!| denotation

  def eparse(s: String): Exp =
    run(expression)(s)

  def tparse(s: String): Typ =
    run(typ)(s)

object printer:

  def prettyPrint(e: Exp | Name): String =
    e match
      case Lam(n, e)    => s"(λ ${prettyPrint(n)} => ${prettyPrint(e)})"
      case App(f, a)    => s"(${prettyPrint(f)} ${prettyPrint(a)})"
      case Hol(n)       => s"?$n"
      case Var(n)       => n
      case Ann(e, t)    => s"(${prettyPrint(e)} : ${prettyPrint(t)})"
      case Prd(e1, e2)  => s"(and ${prettyPrint(e1)} ${prettyPrint(e2)})"
      case Lhs(e)       => s"(lhs ${prettyPrint(e)})"
      case Rhs(e)       => s"(rhs ${prettyPrint(e)})"
      case Sum(i, l, r) => s"(or ${prettyPrint(i)} ${prettyPrint(l)} ${prettyPrint(r)})"
      case name: String => name

  def prettyPrint(t: Typ): String =
    t match
      case Den(n)   => n
      case PTp(a, b) => s"${prettyPrint(a)} ∧ ${prettyPrint(b)}"
      case STp(a, b) => s"${prettyPrint(a)} ∨ ${prettyPrint(b)}"
      case Arr(a, b) => s"(${prettyPrint(a)} -> ${prettyPrint(b)})"

  def prettyPrint(c: Ctx): String = 
    c.map((s, t) => s"\n$s : ${prettyPrint(t)}").mkString

  private def prettyPrint(holes: Holes): String =
    holes
      .map((nr,t,c) => s"[$nr] : ${prettyPrint(t)} in context ${prettyPrint(c)}")
      .mkString
  
  def prettyPrint(g: Goal): String =
    s"""${g.holes.numbers.size} Goals in ${prettyPrint(g.current)}
        |${prettyPrint(g.holes)}
        |${if g.isSolved then "\nSolved." else ""}
      """.stripMargin

  def prettyPrint: State[Goal,Unit] =
    State(g => (println(prettyPrint(g)),g))

object typer:

  import printer.*

  @tailrec
  def check(ctx: Ctx, exp: Exp, typ: Typ, ref: Boolean = false): (Boolean, Ctx) =
    def checkError(msg: String = "unable to check") = 
      sys.error(checkInfo(msg))

    def checkInfo(msg: String = ""): String =
      s"""CHECK ${if ref then "[refining] " + msg else msg}
         |Exp: ${prettyPrint(exp)}
         |Typ: ${prettyPrint(typ)}
         |Ctx: ${prettyPrint(ctx)}
      """.stripMargin.trim

    (exp, typ) match
      case ( Lam(s, e) , Arr(a, b) ) => check(ctx + (s -> a), e, b, ref)
      case ( Lam(x, t) , _         ) => checkError()
      case ( Hol(n)    , _         ) => if ref then (true, ctx + (n.toString -> typ)) else (true, ctx)
      case _                         => if typ == synth(ctx, exp, ref) then (true, ctx) else checkError()

  def synth(ctx: Ctx, exp: Exp, ref: Boolean = false): Typ =
    def synthError(msg: String = "unable to synth") = 
      sys.error(synthInfo(msg))

    def synthInfo(msg: String = "trace"): String =
      s"""SYNTH ${if ref then s"[refining] - $msg" else msg}
         |exp: ${prettyPrint(exp)}
         |ctx: ${prettyPrint(ctx)}
       """.stripMargin

    exp match
      case Lam(_, _)                                 => synthError()
      case Hol(_)                                    => synthError()
      case Ann(e, t) if check(ctx, e, t, ref)._1     => t
      case Prd(a, b)                                 => PTp(synth(ctx, a, ref), synth(ctx, b, ref))
      case App(f, x)                                 =>
        synth(ctx, f, ref) match
          case Arr(a, b) if check(ctx, x, a, ref)._1 => b
          case _                                     => synthError()
      case Var(n)                                    => ctx.getOrElse(n, synthError())
      case Lhs(e)                                    =>
        synth(ctx, e, ref) match
          case PTp(l, _)                             => l
          case _                                     => synthError()
      case Rhs(e)                                    =>
        synth(ctx, e, ref) match
          case PTp(_, r)                             => r
          case _                                     => synthError()
      case _                                         => synthError()

case class State[S, A](run: S => (A, S)):
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State(s => (f(a), s)))

  def map2[B](fa: S => B, fs: S => S): State[S, B] =
    State(s => (fa(s), fs(s)))

  def flatMap[B](f: A => State[S,B]): State[S, B] =
    State(s => { val (a,ss) = run(s) ; f(a).run(ss) })


object State:
  def pure[S, A](a: A): State[S, A] =
    State(s => (a,s))

  def inspect[S, A](f: S => A): State[S, A] =
    State(s => (f(s), s))

  def bimap[S, A](fa: S => A, fs: S => S): State[S, A] =
    State(s => (fa(s), fs(s)))
  
  def get[S]: State[S, S] =
    inspect(identity)

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    State(s => ((), f(s)))

object assistant:

  import printer.*
  import parser.*
  import typer.*
  import State.*

  private def holes(exp: Exp): State[Int, Exp] =
    exp match
      case Lam(s, e)         => holes(e).map(ne => Lam(s, ne))
      case App(f, a)         => for {e1 <- holes(f); e2 <- holes(a)} yield App(e1, e2)
      case v: Var            => pure(v)
      case Ann(e, t)         => holes(e).map(ne => Ann(ne, t))
      case Hol(n) if n == -1 => bimap(n => Hol(n), n => n + 1)
      case h: Hol            => pure(h)
      case Prd(l, r)         => for {nl <- holes(l); nr <- holes(r)} yield Prd(nl ,nr)
      case Lhs(e)            => holes(e).map(ne => Lhs(ne))
      case Rhs(e)            => holes(e).map(ne => Rhs(ne))
      case Sum(i, l, r)      => for {ni <- holes(i); nl <- holes(l); nr <- holes(r)} yield Sum(ni, nl, nr)

  def number(exp: Exp): State[Int, Exp] =
    for { ne <- holes(exp) } yield ne

  def replace(nr: Int, rep: Exp, exp: Exp): Exp =
    exp match
      case Lam(s, e)    => Lam(s, replace(nr, rep, e))
      case App(f, a)    => App(replace(nr, rep, f), replace(nr, rep, a))
      case Var(n)       => Var(n)
      case Ann(e, t)    => Ann(replace(nr, rep, e), t)
      case Hol(-1)      => sys.error(s"unnumbered hole in expression: $exp")
      case Hol(n)       => if n == nr then rep else Hol(n)
      case Prd(e1, e2)  => Prd(replace(nr, rep, e1), replace(nr, rep, e2))
      case Lhs(e)       => Lhs(replace(nr, rep, e))
      case Rhs(e)       => Rhs(replace(nr, rep, e))
      case Sum(i, l, r) => Sum(replace(nr, rep, i), replace(nr, rep, l), replace(nr, rep, r))

  def task(task: String): State[Goal,Exp] =
    State(_ =>
      val goal = Goal(task)
      (goal.current, goal)
    )

  def solved: State[Goal, Boolean] =
    State(g => (g.isSolved, g))

  def refine(hole: Int, refinement: String): State[Goal, Goal] =
    val exp: Exp =
      eparse(refinement)
      
    State(goal =>
      if (goal.trace) println(prettyPrint(goal))

      val (typ, c)  = goal.holes.get(hole)
      val _         = check(c, exp, typ)
      val (ne, nc)  = number(exp).run(goal.nextNr)
      val (_, ctx)  = check(c, ne, typ, ref = true)

      val ngoal =
        if nc != hole + 1 then
          val nhole    = ( hole + 1 , ctx((hole + 1).toString) ,  ctx )
          val nexp     = replace(hole, ne, goal.current)
          val nholes   = goal.holes - hole + nhole
          goal.copy(nexp, nholes, nc)
        else
          goal.copy(ne, Holes.empty, -1, true)

      (ngoal,ngoal)
    )
