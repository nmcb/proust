package proust
package test

import org.junit.Test
import org.junit.Assert._

class ProustTests {

import parser._
import typer._

  def proof(p: String): Boolean =
    scala.util
      .Try(synth(Map.empty, eparse(p)))
      .map(_ => true)
      .getOrElse(false)

  @Test def testProof(): Unit =
    proof("((λ x => (λ y => x)) : (A -> (B -> A)))")
}
