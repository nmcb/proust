package proust
package test

import org.junit.Test
import org.junit.Assert._

class PrinterTests {

  import printer._

  @Test def testPrintExp(): Unit = { 
    assertEquals( "(λ a => b)" , pprint(Lam(Sym("a"),Sym("b"))))
    assertEquals( "(a b)"      , pprint(App(Sym("a"),Sym("b"))))
    assertEquals( "(a : A)"    , pprint(Ann(Sym("a"),Den("A"))))
    assertEquals( "a"          , pprint(Sym("a")))
  }

  @Test def testPrintTyp(): Unit = { 
    assertEquals( "(A -> B)" , pprint(Arr(Den("A"),Den("B"))))
    assertEquals( "A"        , pprint(Den("A")))
  }
}