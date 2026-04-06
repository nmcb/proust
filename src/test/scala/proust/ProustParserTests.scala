package proust

import org.junit.Assert.assertEquals
import org.junit.Test

class ProustParserTests:

  import Exp.*
  import Typ.*
  import parsing.*
  import P.*
  import parser.*

  @Test def testProustParserName(): Unit =
    assertEquals("a", run(name)("a"))
    assertEquals("ab", run(name)("ab"))
    assertEquals("a-b", run(name)("a-b"))

  @Test def testProustParserTypeName(): Unit =
    assertEquals("A", run(typeName)("A"))
    assertEquals("AB", run(typeName)("AB"))
    assertEquals("A-B", run(typeName)("A-B"))

  @Test def testProustParserVariable(): Unit =
    assertEquals(Var("a"), run(variable)("a "))
    assertEquals(Var("ab"), run(variable)("ab \t"))
    assertEquals(Var("a-b"), run(variable)("a-b \n"))

  @Test def testProustParserHole(): Unit =
    assertEquals(Hol(-1), run(hole)("?"))
    assertEquals(Hol(1), run(hole)("?1"))
    assertEquals(Hol(12), run(hole)("?12"))

  @Test def testProustParserApplication(): Unit =
    // formal application form
    assertEquals(App(Var("a"), Var("b")), run(application)("(a b)"))
    assertEquals(App(Hol(-1), Hol(-1)), run(application)("(? ?)"))
    assertEquals(App(App(Var("a"), Var("b")), Var("c")), run(application)("((a b) c)"))
    assertEquals(App(Var("a"), App(Var("b"), Var("c"))), run(application)("(a (b c))"))

  @Test def testProustParserLambda(): Unit =
    assertEquals(Lam("a", Var("b")), run(lambda)("(λ a => b)"))
    assertEquals(Lam("a", Lam("b", Var("c"))), run(lambda)("(λ a => (λ b => c))"))

  @Test def testProustParserAnnotation(): Unit =
    assertEquals(Ann(Var("a"), Den("A")), run(annotation)("(a : A)"))

  @Test def testProustParserProduct(): Unit =
    assertEquals(Prd(Var("a"), Var("b")), run(product)("(and a b)"))

  @Test def testProustParserLhs(): Unit =
    assertEquals(Lhs(Prd(Var("a"), Var("b"))), run(lhs)("(lhs (and a b))"))

  @Test def testProustParserRhs(): Unit =
    assertEquals(Rhs(Prd(Var("a"), Var("b"))), run(rhs)("(rhs (and a b))"))

  @Test def testProustParserExpr(): Unit =
    assertEquals(Lam("a", Var("b")), run(expression)("(λ a => b)"))
    assertEquals(App(Var("a"), Var("b")), run(expression)("(a  b)"))
    assertEquals(Var("a"), run(expression)("a"))
    assertEquals(Ann(Var("a"), Den("A")), run(expression)("(a : A)"))
    assertEquals(Prd(Var("a"), Var("b")), run(expression)("(and a b)"))
    assertEquals(Lhs(Prd(Var("a"), Var("b"))), run(expression)("(lhs (and a b))"))
    assertEquals(Rhs(Prd(Var("a"), Var("b"))), run(expression)("(rhs (and a b))"))
    // human-readable form employing the intuition that application is left-associative
    assertEquals(App(App(Var("a"), Var("b")), Var("c")), run(expression)("(a b c)"))

  @Test def testProustParserDenotation(): Unit =
    assertEquals(Den("A"), run(denotation)("A "))
    assertEquals(Den("AB"), run(denotation)("AB \t"))
    assertEquals(Den("A-B"), run(denotation)("A-B \n"))

  @Test def testProustParserArrow(): Unit =
    assertEquals(Arr(Den("A"), Den("B")), run(arrow)("(A -> B)"))
    assertEquals(Arr(Den("A"), Arr(Den("B"), Den("C"))), run(arrow)("(A -> (B -> C))"))

  @Test def testProustParserProductType(): Unit =
    assertEquals(PTp(Den("A"), Den("B")), run(andtyp)("(A ∧ B)"))

  @Test def testProustParserSumType(): Unit =
    assertEquals(STp(Den("A"), Den("B")), run(ortyp)("(A ∨ B)"))

  @Test def testProustParserTyp(): Unit =
    assertEquals(Den("A"), run(typ)("A"))
    assertEquals(Arr(Den("A"), Den("B")), run(typ)("(A -> B)"))
    assertEquals(PTp(Den("A"), Den("B")), run(typ)("(A ∧ B)"))
    assertEquals(STp(Den("A"), Den("B")), run(typ)("(A ∨ B)"))
    // human-readable form employing the observation that application is right-associative
    assertEquals(Arr(Den("A"), Arr(Den("B"), Den("C"))), run(typ)("(A -> B -> C)"))
    // product and sum types have higher precedence than arrow
    assertEquals(Arr(Den("A"), PTp(Den("B"), Den("C"))), run(typ)("(A -> (B ∧ C))"))
    assertEquals(Arr(Den("A"), STp(Den("B"), Den("C"))), run(typ)("(A -> (B ∨ C))"))
