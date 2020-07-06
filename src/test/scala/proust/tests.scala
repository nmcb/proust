package proust

import org.junit.Test
import org.junit.Assert._

class ParserTests {

  import P._

  @Test def testSatisfy(): Unit = {
    assertEquals(run(satisfy(_ == 'c'))("c"), 'c')
  }

  @Test def testChar(): Unit = {
    assertEquals(run(char('c'))("c"), 'c')
  }

  @Test def testString(): Unit = {
    assertEquals(run(string("abcd"))("abcd"), "abcd")
  }

  @Test def testSpaces(): Unit = {
    assertEquals(run(string(" \t\n\r"))(" \t\n\r"), " \t\n\r")
  }

  @Test def testDigit(): Unit = {
    assertEquals(run(digit)("1"), '1')
  }

}