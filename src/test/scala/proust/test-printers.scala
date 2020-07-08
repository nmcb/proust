package proust
package test

import org.junit.Test
import org.junit.Assert._

class PrinterTests {

  import printer._

  @Test def testPrintExp(): Unit = { 
    assertEquals( "(λ a => b)" , print(Lam(Sym("a"),Sym("b"))))
    assertEquals( "(a b)"      , print(App(Sym("a"),Sym("b"))))
    assertEquals( "(a : A)"    , print(Ann(Sym("a"),Den("A"))))
    assertEquals( "a"          , print(Sym("a")))
  }

  @Test def testPrintTyp(): Unit = { 
    assertEquals( "(A -> B)" , print(Arr(Den("A"),Den("B"))))
    assertEquals( "A"        , print(Den("A")))
  }
}