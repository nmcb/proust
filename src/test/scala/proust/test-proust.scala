package proust
package test

import org.junit.Test
import org.junit.Assert._

class ProustTests {

  import disjoining.option._
  import Opt._

  import printer._
  import parser._
  import typer._
  import assistent._

  private def proof(p: String): Boolean =
    scala.util
      .Try(synth(Map.empty, eparse(p)))
      .map(_ => true)
      .getOrElse(false)

  @Test def testProof(): Unit = {
    proof("((λ x => (λ f => (f x))) : (A -> ((A -> B) -> B)))")
    proof("((λ x => (λ y => x)) : (A -> (B -> A)))")
  }

  @Test def testNumber(): Unit = {
    val exp = Lam(Var("a"),App(Hol(0),Hol(1)))
    val fix = Lam(Var("a"),App(Hol( ),Hol( )))
    assertEquals( (exp, 2) , number(fix, 0))
  }

  @Test def testRefine(): Unit = {

    val hypothesis: Goal = Goal("(A -> (B -> A))")

    val proof: State[Goal,Unit] =
      (for {
        g1 <- refine( 0 , "(λ x => ?)" )
        g2 <- refine( 1 , "(λ y => ?)" )
        g3 <- refine( 2 , "x"          )

        _  <- State.unit(assert(g3.isSolved))
        } yield ())

    proof.run(hypothesis)
  }

  @Test def testExercise01(): Unit = {

    val hypothesis: Goal = Goal("((A -> B -> C) -> (A -> B) -> (A -> C))")

    val proof: State[Goal,Unit] =
      (for {
        g1 <- refine( 0 , "(λ x => ?)" )
        g2 <- refine( 1 , "(λ y => ?)" )
        g3 <- refine( 2 , "(λ z => ?)" )
        g4 <- refine( 3 , "((x z) (y z))" )

        _  <- State.unit(assert(g4.isSolved))
        } yield ())

    proof.run(hypothesis)
  }

  @Test def testExercise02(): Unit = {

    val hypothesis: Goal = Goal("((A -> B) -> (A -> C) -> (A -> B -> C))")

    val proof: State[Goal,Unit] =
      (for {
        g1 <- refine( 0 , "(λ x => ?)" )
        g2 <- refine( 1 , "(λ y => ?)" )
        g3 <- refine( 2 , "(λ z => ?)" )
        g4 <- refine( 3 , "(λ w => ?)" )
        g5 <- refine( 4 , "(y z)" )

        _  <- State.unit(assert(g5.isSolved))
        } yield ())

    proof.run(hypothesis)
  }

  @Test def testExercise03(): Unit = {

    val hypothesis: Goal = Goal("((B -> C) -> (A -> B) -> (A -> C))")

    val proof: State[Goal,Unit] =
      (for {
        g1 <- refine( 0 , "(λ x => ?)" )
        g2 <- refine( 1 , "(λ y => ?)" )
        g3 <- refine( 2 , "(λ z => ?)" )  
        g4 <- refine( 3 , "(x (y z))" )

        _  <- State.unit(assert(g4.isSolved))
        } yield ())

    proof.run(hypothesis)
  }

  @Test def testProduct(): Unit = {

    val hypothesis: Goal = Goal("(A -> B -> (A && B))")

    val proof: State[Goal,Unit] =
      (for {
        g1 <- refine( 0 , "(λ a => ?)" )
        g2 <- refine( 1 , "(λ b => ?)" )
        g3 <- refine( 2 , "(* a b)" )

        _  <- State.unit(assert(g3.isSolved))
        } yield ())

    proof.run(hypothesis)
  }

  @Test def testTrivialProductProof(): Unit =
    proof("(λ a => (λ b => (* a b))) : (A -> B -> A && B)")

  @Test def testTrivialProductFst(): Unit =
    proof("(fst (* (a : A) (b : B))) : A)")
  
  @Test def testTrivialProductSnd(): Unit =
    proof("(snd (* (a : A) (b : B))) : B)")

  @Test def testTrivialProductCommutative(): Unit =
    proof("((λ c => (* (snd c) (fst c))) : ((A && B) -> (B && A)))")
  
  
  @Test def testExercise21(): Unit = {

    val hypothesis: Goal = Goal("(((A && B) -> C) -> (A -> B -> C))")

    val proof: State[Goal,Unit] =
      (for {
        g1 <- refine( 0 , "(λ x => ?)"  )
        g2 <- refine( 1 , "(λ y => ?)"  )
        g3 <- refine( 2 , "(λ z => ?)"  )  
        g4 <- refine( 3 , "(x (* y z))" )

        _  <- State.unit(assert(g4.isSolved))
        } yield ())

    proof.run(hypothesis)
  }  

  @Test def testExercise22(): Unit = {

    val hypothesis: Goal = Goal("((A -> B -> C) -> ((A && B) -> C))")

    val proof: State[Goal,Unit] =
      (for {
        g1 <- refine( 0 , "(λ x => ?)" )
        g2 <- refine( 1 , "(λ y => ?)" )
        g3 <- refine( 2 , "((x (fst y)) (snd y))" )  

        _  <- State.unit(assert(g3.isSolved))
        } yield ())

    proof.run(hypothesis)
  }  

  @Test def testExercise23(): Unit = {

    val hypothesis: Goal = Goal("((A -> B) -> ((A && C) -> (B && C)))")

    val proof: State[Goal,Unit] =
      (for {
        g1 <- refine( 0 , "(λ x => ?)" )
        g2 <- refine( 1 , "(λ y => ?)" )
        g3 <- refine( 2 , "(* (x (fst y)) (snd y))" )  

        _  <- State.unit(assert(g3.isSolved))
        } yield ())

    proof.run(hypothesis)
  }  

  @Test def testExercise24(): Unit = {

    val hypothesis: Goal = Goal("(((A -> B) && (C -> D)) -> ((A && C) -> (B && D)))")

    val proof: State[Goal,Unit] =
      (for {
        g1 <- refine( 0 , "(λ x => ?)" )
        g2 <- refine( 1 , "(λ y => ?)" )
        g3 <- refine( 2 , "(* ((fst x) (fst y)) ((snd x) (snd y)))" )  

        _  <- State.unit(assert(g3.isSolved))
        } yield ())

    proof.run(hypothesis)
  }  


}