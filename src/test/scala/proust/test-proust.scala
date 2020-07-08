package proust
package test

import org.junit.Test
import org.junit.Assert._

class ProustTests {

import parsing._
import parser._
import typer._

  def proof(p: String): Unit =
    synth(Map.empty, parse(p))

  @Test def testProof(): Unit =
    proof("((λ x => (λ y => x)) : (A -> (B -> A)))")
}