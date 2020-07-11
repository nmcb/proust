package proust
package test

import org.junit.Test
import org.junit.Assert._

class DisjoiningTests {

  import disjoining._
  import option._

  @Test def testOptGetOrElse(): Unit = { 
    assertEquals( 1 , Opt.non.getOrElse(1))
    assertEquals( 1 , Opt(1).getOrElse(2))
  }
}