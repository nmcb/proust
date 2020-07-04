package proust

import org.junit.Test
import org.junit.Assert._

class ParserTests {

  import P._

  @Test def test(): Unit = {
    assertEquals(run(satisfy(_ == 'c'))("c"), 'c')
  }
}