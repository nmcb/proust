package proust

import org.junit.Assert.assertEquals
import org.junit.Test

class CalculatorParserTests:

  import parsing.*
  import P.*
  import calculator.*
  import Expr.*

  @Test def testInt(): Unit =
    assertEquals(Lit(1), run(int)("1"))
    assertEquals(Lit(-1), run(int)("-1"))
    assertEquals(Lit(10), run(int)("10"))
    assertEquals(Lit(-10), run(int)("-10"))

  @Test def testFactor(): Unit =
    assertEquals(Lit(1), run(factor)("1"))
    assertEquals(Lit(1), run(factor)("(1)"))

  @Test def testTerm(): Unit =
    assertEquals(Lit(2), run(term)("2"))

    assertEquals(Mul(Lit(1), Lit(2)), run(term)("1*2"))
    assertEquals(Div(Lit(1), Lit(2)), run(term)("1/2"))

    assertEquals(Mul(Mul(Lit(1), Lit(2)), Lit(3)), run(term)("1*2*3"))
    assertEquals(Div(Div(Lit(1), Lit(2)), Lit(3)), run(term)("1/2/3"))

    assertEquals(Div(Mul(Lit(1), Lit(2)), Lit(3)), run(term)("1*2/3"))
    assertEquals(Mul(Div(Lit(1), Lit(2)), Lit(3)), run(term)("1/2*3"))

  @Test def testExpr(): Unit =
    assertEquals(Lit(1), run(expr)("1"))

    assertEquals(Add(Lit(1), Lit(2)), run(expr)("1+2"))
    assertEquals(Sub(Lit(1), Lit(2)), run(expr)("1-2"))

    assertEquals(Add(Add(Lit(1), Lit(2)), Lit(3)), run(expr)("1+2+3"))
    assertEquals(Sub(Sub(Lit(1), Lit(2)), Lit(3)), run(expr)("1-2-3"))

    assertEquals(Sub(Add(Lit(1), Lit(2)), Lit(3)), run(expr)("1+2-3"))
    assertEquals(Add(Sub(Lit(1), Lit(2)), Lit(3)), run(expr)("1-2+3"))

  @Test def testCalculatorEval(): Unit =
    assertEquals(1, calculator.eval("1"))
    assertEquals(3, calculator.eval("1+2"))
    assertEquals(6, calculator.eval("1+2+3"))
    assertEquals(1, calculator.eval("2-1"))
    assertEquals(4, calculator.eval("2*2"))
    assertEquals(1, calculator.eval("2/2"))
    assertEquals(7, calculator.eval("1+(2*3)"))
    assertEquals(7, calculator.eval("1+2*3"))
    assertEquals(9, calculator.eval("(1+2)*3"))
    assertEquals(6, calculator.eval("(1+2)*(4/2)"))
