package proust
package test

import org.junit.Test
import org.junit.Assert._

class DisjcunctionTests {

  import disjoining._
  import option._

  @Test def testOptionGetOrElse(): Unit = { 
    assertEquals( 1 , N.getOrElse(1))
    assertEquals( 1 , S(1).getOrElse(2))
  }
}