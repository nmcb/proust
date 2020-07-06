package proust

import org.junit.Test
import org.junit.Assert._

class ParserTests {

  import P._

  @Test def testFlatMap(): Unit = {
    assertEquals(run(
      for {
        a <- string("a")
        b <- string("b")
      } yield a + b
    )("ab"), "ab")
  }

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
    assertEquals(run(spaces)(" \t\n\r"), " \t\n\r")
  }

  @Test def testToken(): Unit = {
    assertEquals(run(token(string("abc")))("abc  "), "abc")
  }

  @Test def testDigit(): Unit = {
    assertEquals(run(digit)("1"), '1')
  }

  @Test def testNumber(): Unit = {
    assertEquals(run(number)("-1"), -1)
    assertEquals(run(number)("10"), 10)
  }

  @Test def testParens(): Unit = {
    assertEquals(run(parens(number))("(-1)"), -1)
    assertEquals(run(parens(number))("(10)"), 10)
    assertEquals(run(parens(number))("( -1)"), -1)
    assertEquals(run(parens(number))("( 10)"), 10)
  }

  @Test def testSome(): Unit = {
    assertEquals( List('0', '1', '2') , run(digit.some)("012"))
    assertEquals( List('a')           , run(char('a').some)("a"))
  }

  @Test def testMany(): Unit = {
    assertEquals( List('0', '1', '2') , run(digit.many)("012"))
    assertEquals( List()              , run(char('a').many)(""))
  }

  // calculator peersers

  import calculator._

  @Test def testInt(): Unit = {
    assertEquals( Lit(1) , run(int)("1")  )
    assertEquals( Lit(-1) , run(int)("-1")  )
    assertEquals( Lit(10) , run(int)("10")  )
    assertEquals( Lit(-10) , run(int)("-10")  )
  }

  @Test def testFactor(): Unit = {
    assertEquals( Lit(1) , run(factor)("1")  )
    assertEquals( Lit(1) , run(factor)("(1)")  )
  }

  @Test def testCalculator(): Unit = {
    assertEquals( 1 , calculator.eval("1")  )
    assertEquals( 3 , calculator.eval("1+2")  )
    assertEquals( 1 , calculator.eval("2-1")  )
    assertEquals( 4 , calculator.eval("2*2")  )
    assertEquals( 1 , calculator.eval("2/2")  )
    assertEquals( 7 , calculator.eval("(1+(2*3))")  )
    assertEquals( 3 , calculator.eval("(1+2)")  )
    assertEquals( 6 , calculator.eval("(1+2)*(4-2)")  )
  }

}