package proust
package test

import org.junit.Test
import org.junit.Assert._

class ProustTests {

  import disjoining.option._
  import Opt._

  import parser._
  import typer._
  import assistent._


  private def proof(p: String): Boolean =
    scala.util
      .Try(synth(Map.empty, eparse(p)))
      .map(_ => true)
      .getOrElse(false)

  // @Test def testProof(): Unit =
  //   proof("((λ x => (λ y => x)) : (A -> (B -> A)))")

  // @Test def testNumber(): Unit = {
  //   val exp = Lam(Sym("a"),App(Hol(The(0)),Hol(The(1))))
  //   val fix = Lam(Sym("a"),App(Hol(Opt.empty),Hol(Opt.empty)))
  //   assertEquals( (exp, 2) , number(fix, 0))
  // }

  private def inspect[S,A](test: S => Unit): State[S,Unit] =
    State(s => { test(s) ; ((),s) })

  // @Test def testTask(): Unit = {
  //   val n      = 0 
  //   val fix    = "(A -> (B -> A))"
  //   val t      = tparse(fix)
  //   val (e, c) = number(eparse(s"(? : $fix)"), n)
  //   val exp    = Goal(e, Holes.init(n, t), c)
  //   inspect(task(fix))(act => assertEquals(exp, act))
  // }

  @Test def testRefine(): Unit = {

    val goal: Goal = task("(A -> (B -> A))" )

    val session: State[Goal,Unit] =
      for {
        g1 <- refine( 0 , "(λ x => ?)" )
        g2 <- refine( 1 , "(λ y => ?)" )
        g3 <- refine( 2 , "x"          )
        _  <- State.unit({
                println(s"G3: $g3")
                assert(g3.isSolved)
              })
        } yield ()

    session.run(goal)
  }
}