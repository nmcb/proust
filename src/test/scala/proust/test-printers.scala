package proust
package test

import org.junit.Test
import org.junit.Assert.*

class PrinterTests:

  import Typ.*
  import printer.*

  @Test def testPrintExp(): Unit =
    assertEquals("(λ a => b)", ppexp(Lam(Var("a"),Var("b"))))
    assertEquals("(a b)"     , ppexp(App(Var("a"),Var("b"))))
    assertEquals("(a : A)"   , ppexp(Ann(Var("a"),Den("A"))))
    assertEquals("a"         , ppexp(Var("a")))

  @Test def testPrintTyp(): Unit =
    assertEquals("(A -> B)", pptyp(Arr(Den("A"),Den("B"))))
    assertEquals("A"       , pptyp(Den("A")))
