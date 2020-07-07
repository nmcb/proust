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

  private def gt: P[Char => Char => Char] =
    unit(x => y => if (x > y) then x else y)

  @Test def testChainl1(): Unit = {
    assertEquals(('1'), run(digit.chainl1(gt))("1"))
    assertEquals(('2'), run(digit.chainl1(gt))("12"))
    assertEquals(('3'), run(digit.chainl1(gt))("123"))
   }

   @Test def testChainl(): Unit = {
    assertEquals(('0'), run(digit.chainl(gt)('0'))(""))
    assertEquals(('1'), run(digit.chainl(gt)('0'))("1"))
    assertEquals(('2'), run(digit.chainl(gt)('0'))("12"))
    assertEquals(('3'), run(digit.chainl(gt)('0'))("123"))
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

  @Test def testOneOrMore(): Unit = {
    assertEquals( List('0', '1', '2') , run(digit.oneOrMore)("012"))
    assertEquals( List('a')           , run(char('a').oneOrMore)("a"))
  }

  @Test def testZeroOrMore(): Unit = {
    assertEquals( List('0', '1', '2') , run(digit.zeroOrMore)("012"))
    assertEquals( List()              , run(char('a').zeroOrMore)(""))
  }

  // calculator

  import calculator._

  @Test def testInt(): Unit = {
    assertEquals( Lit(1) , run(int)("1"))
    assertEquals( Lit(-1) , run(int)("-1"))
    assertEquals( Lit(10) , run(int)("10"))
    assertEquals( Lit(-10) , run(int)("-10"))
  }

  @Test def testFactor(): Unit = {
    assertEquals( Lit(1) , run(factor)("1"))
    assertEquals( Lit(1) , run(factor)("(1)"))
  }

  @Test def testTerm(): Unit = {
    assertEquals( Lit(2) , run(term)("2"))

    assertEquals( Mul(Lit(1),Lit(2)) , run(term)("1*2"))
    assertEquals( Div(Lit(1),Lit(2)) , run(term)("1/2"))

    assertEquals( Mul(Mul(Lit(1),Lit(2)),Lit(3)) , run(term)("1*2*3"))
    assertEquals( Div(Div(Lit(1),Lit(2)),Lit(3)) , run(term)("1/2/3"))

    assertEquals( Div(Mul(Lit(1),Lit(2)),Lit(3)) , run(term)("1*2/3"))
    assertEquals( Mul(Div(Lit(1),Lit(2)),Lit(3)) , run(term)("1/2*3"))
  }

  @Test def testExpr(): Unit = {
    assertEquals( Lit(1) , run(expr)("1"))

    assertEquals( Add(Lit(1),Lit(2)) , run(expr)("1+2"))
    assertEquals( Sub(Lit(1),Lit(2)) , run(expr)("1-2"))

    assertEquals( Add(Add(Lit(1),Lit(2)),Lit(3)) , run(expr)("1+2+3"))
    assertEquals( Sub(Sub(Lit(1),Lit(2)),Lit(3)) , run(expr)("1-2-3"))

    assertEquals( Sub(Add(Lit(1),Lit(2)),Lit(3)) , run(expr)("1+2-3"))
    assertEquals( Add(Sub(Lit(1),Lit(2)),Lit(3)) , run(expr)("1-2+3"))
  }

  @Test def testCalculator(): Unit = {
    assertEquals( 1 , calculator.eval("1"))
    assertEquals( 3 , calculator.eval("1+2"))
    assertEquals( 6 , calculator.eval("1+2+3"))
    assertEquals( 1 , calculator.eval("2-1"))
    assertEquals( 4 , calculator.eval("2*2"))
    assertEquals( 1 , calculator.eval("2/2"))
    assertEquals( 7 , calculator.eval("1+(2*3)"))
    assertEquals( 7 , calculator.eval("1+2*3"))
    assertEquals( 9 , calculator.eval("(1+2)*3"))
    assertEquals( 6 , calculator.eval("(1+2)*(4/2)"))
  }

  // proust


}