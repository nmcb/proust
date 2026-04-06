package proust

import org.junit.Test
import org.junit.Assert.*

class PrinterTests:

  import Typ.*
  import Exp.*
  import printer.*

  @Test def testPrintExp(): Unit =
    assertEquals("(λ a => b)", prettyPrint(Lam("a",Var("b"))))
    assertEquals("(a b)"     , prettyPrint(App(Var("a"),Var("b"))))
    assertEquals("(a : A)"   , prettyPrint(Ann(Var("a"),Den("A"))))
    assertEquals("a"         , prettyPrint(Var("a")))

  @Test def testPrintTyp(): Unit =
    assertEquals("(A -> B)", prettyPrint(Arr(Den("A"),Den("B"))))
    assertEquals("A"       , prettyPrint(Den("A")))
