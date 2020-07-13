package proust
package test

import org.junit._
import org.junit.Assert._

class ParserTests {

  import sequencing._
  import parsing._
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
    assertEquals( '2' , run(digit.chainl1(gt))("12"))
    assertEquals( '3' , run(digit.chainl1(gt))("123"))
  }

   @Test def testChainl(): Unit = {
    assertEquals( '0' , run(digit.chainl(gt)('0'))(""))
    assertEquals( '1' , run(digit.chainl(gt)('0'))("1"))
    assertEquals( '2' , run(digit.chainl(gt)('0'))("12"))
    assertEquals( '3' , run(digit.chainl(gt)('0'))("123"))
   }

  @Test def testSatisfy(): Unit = {
    assertEquals( 'c' , run(satisfy(_ == 'c'))("c"))
  }

  @Test def testChar(): Unit = {
    assertEquals( 'c' , run(char('c'))("c"))
  }

  @Test def testString(): Unit = {
    assertEquals( "abcd", run(string("abcd"))("abcd"))
  }

  @Test def testSpaces(): Unit = {
    assertEquals( " \t\n\r" , run(spaces)(" \t\n\r"))
  }

  @Test def testToken(): Unit = {
    assertEquals( "abc" , run(token(string("abc")))("abc  "))
  }

  @Test def testDigit(): Unit = {
    assertEquals( '1' , run(digit)("1"))
  }

  @Test def testNumber(): Unit = {
    assertEquals( -1 , run(number)("-1"))
    assertEquals( 10 , run(number)("10"))
  }

  @Test def testParens(): Unit = {
    assertEquals( -1 , run(parens(number))("(-1)"))
    assertEquals( 10 , run(parens(number))("(10)"))
    assertEquals( -1 , run(parens(number))("( -1)"))
    assertEquals( 10 , run(parens(number))("( 10)"))
  }

  @Test def testOneOrMore(): Unit = {
    assertEquals( Seq('0')           , run(digit.oneOrMore)("0"))
    assertEquals( Seq('0', '1', '2') , run(digit.oneOrMore)("012"))
  }

  @Test def testZeroOrMore(): Unit = {
    assertEquals( Seq()              , run(digit.zeroOrMore)(""))
    assertEquals( Seq('0', '1', '2') , run(digit.zeroOrMore)("012"))
  }
}

class CalculatorParserTests {

  import parsing._
  import calculator._
  import P._

  @Test def testInt(): Unit = {
    assertEquals( Lit(1)   , run(int)("1"))
    assertEquals( Lit(-1)  , run(int)("-1"))
    assertEquals( Lit(10)  , run(int)("10"))
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

  @Test def testCalculatorEval(): Unit = {
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
}

class ProustParserTests {

  import disjoining.option._
  import parsing._
  import P._
  import parser._

  @Test def testProustParserName(): Unit = {
    assertEquals( "a"   , run(name)("a"))
    assertEquals( "ab"  , run(name)("ab"))
    assertEquals( "a-b" , run(name)("a-b"))
  }

  @Test def testProustParserTypeName(): Unit = {
    assertEquals( "A"   , run(typeName)("A"))
    assertEquals( "AB"  , run(typeName)("AB"))
    assertEquals( "A-B" , run(typeName)("A-B"))
  }

  @Test def testProustParserVariable(): Unit = {
    assertEquals( Var("a")   , run(variable)("a "))
    assertEquals( Var("ab")  , run(variable)("ab \t"))
    assertEquals( Var("a-b") , run(variable)("a-b \n"))
  }

  @Test def testProustParserHole(): Unit = {
    assertEquals( Hol(Opt.non) , run(hole)("?"))
  }

  @Test def testProustParserApplication(): Unit = {
    // formal application form
    assertEquals( App(Var("a"),Var("b"))               , run(application)("(a b)"))
    assertEquals( App(App(Var("a"),Var("b")),Var("c")) , run(application)("((a b) c)"))
    assertEquals( App(Var("a"),App(Var("b"),Var("c"))) , run(application)("(a (b c))"))
  }  

  @Test def testProustParserLambda(): Unit = {
    assertEquals( Lam(Var("a"),Var("b"))               , run(lambda)("(λ a => b)"))
    assertEquals( Lam(Var("a"),Lam(Var("b"),Var("c"))) , run(lambda)("(λ a => (λ b => c))"))
  }  

  @Test def testProustParserAnnotation(): Unit = {
    assertEquals( Ann(Var("a"),Den("A")) , run(annotation)("(a : A)"))
    assertEquals( Ann(Var("a"),Den("A")) , run(annotation)("(a : A)"))
  }  

  @Test def testProustParserExpr(): Unit = {
    assertEquals( Lam(Var("a"),Var("b")) , run(expression)("(λ a => b)"))
    assertEquals( App(Var("a"),Var("b")) , run(expression)("(a  b)"))
    assertEquals( Var("a")               , run(expression)("a"))
    assertEquals( Ann(Var("a"),Den("A")) , run(expression)("(a : A)"))
    // human readable form employing the observation that application is left-associative
    assertEquals( App(App(Var("a"),Var("b")),Var("c")) , run(expression)("(a b c)"))
  }  

  @Test def testProustParserDenotation(): Unit = {
    assertEquals( Den("A")   , run(denotation)("A "))
    assertEquals( Den("AB")  , run(denotation)("AB \t"))
    assertEquals( Den("A-B") , run(denotation)("A-B \n"))
  }

  @Test def testProustParserArrow(): Unit = { 
    assertEquals( Arr(Den("A"),Den("B"))               , run(arrow)("(A -> B)"))
    assertEquals( Arr(Den("A"),Arr(Den("B"),Den("C"))) , run(arrow)("(A -> (B -> C))"))
  }  

  @Test def testProustParserTyp(): Unit = { 
    assertEquals( Den("A")               , run(typ)("A"))
    assertEquals( Arr(Den("A"),Den("B")) , run(typ)("(A -> B)"))
    // human readable form employing the observation that application is right-associative
    assertEquals( Arr(Den("A"),Arr(Den("B"),Den("C"))) , run(typ)("(A -> B -> C)"))
  }  
}
