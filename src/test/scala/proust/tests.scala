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

  // @Test def testFlatMap(): Unit = {
  //   assertEquals(run(satisfy(_ == '+').flatMap(_.toInt))("c"), 1)
  // }

  // @Test def testOneOf(): Unit = {
  //   val c = run(oneOf("abcd"))("c")
  //   assertEquals(c, 'c')

  //   val d = run(combine(oneOf("abcd"), oneOf("abcd")))("cc")
  //   println(d)
  //   assertEquals(d, 'd')
  // }
}