package proust

import org.junit.Test
import org.junit.Assert._

class ParserTests {

  import P._

  @Test def testSatisfy(): Unit = {
    assertEquals(run(satisfy(_ == 'c'))("c"), 'c')
  }

  @Test def testOneOf(): Unit = {
    assertEquals(run(oneOf("abcd"))("c"), 'c')
  }
}