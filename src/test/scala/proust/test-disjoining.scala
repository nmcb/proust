package proust
package test

import org.junit.Test
import org.junit.Assert.*

class DisjoiningTests:

  import disjoining.*
  import option.*

  @Test def testOptGetOrElse(): Unit =
    assertEquals(1, Opt.non.getOrElse(1))
    assertEquals(1, Opt(1).getOrElse(2))
