package proust

import org.junit.*
import org.junit.Assert.*

class ParserTests:

  import sequencing.*
  import parsing.*
  import P.*

  @Test def testFlatMap(): Unit =
    assertEquals(run(
      for
        a <- string("a")
        b <- string("b")
      yield
        a + b
    )("ab"), "ab")

  private def gt: P[Char => Char => Char] =
    unit(x => y => if (x > y) x else y)

  @Test def testChainLeft1(): Unit =
    assertEquals('2', run(digit.chainLeft1(gt))("12"))
    assertEquals('3', run(digit.chainLeft1(gt))("123"))

  @Test def testChainLeft(): Unit =
    assertEquals('0', run(digit.chainLeft(gt)('0'))(""))
    assertEquals('1', run(digit.chainLeft(gt)('0'))("1"))
    assertEquals('2', run(digit.chainLeft(gt)('0'))("12"))
    assertEquals('3', run(digit.chainLeft(gt)('0'))("123"))

  @Test def testSatisfy(): Unit =
    assertEquals('c', run(satisfy(_ == 'c'))("c"))

  @Test def testChar(): Unit =
    assertEquals('c', run(char('c'))("c"))

  @Test def testString(): Unit =
    assertEquals("abcd", run(string("abcd"))("abcd"))

  @Test def testSpaces(): Unit =
    assertEquals(" \t\n\r", run(spaces)(" \t\n\r"))

  @Test def testToken(): Unit =
    assertEquals("abc", run(token(string("abc")))("abc  "))

  @Test def testDigit(): Unit =
    assertEquals('1', run(digit)("1"))

  @Test def testNumber(): Unit =
    assertEquals(-1, run(number)("-1"))
    assertEquals(10, run(number)("10"))

  @Test def testParens(): Unit =
    assertEquals(-1, run(parens(number))("(-1)"))
    assertEquals(10, run(parens(number))("(10)"))
    assertEquals(-1, run(parens(number))("( -1)"))
    assertEquals(10, run(parens(number))("( 10)"))

  @Test def testOneOrMore(): Unit =
    assertEquals(Seq('0')           , run(digit.oneOrMore)("0"))
    assertEquals(Seq('0', '1', '2') , run(digit.oneOrMore)("012"))

  @Test def testZeroOrMore(): Unit =
    assertEquals(Seq()              , run(digit.zeroOrMore)(""))
    assertEquals(Seq('0', '1', '2') , run(digit.zeroOrMore)("012"))
