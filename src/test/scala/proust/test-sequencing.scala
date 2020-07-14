package proust
package test

import org.junit.Test
import org.junit.Assert._

class SequencingTests {

  import sequencing._
  import Seq._

  @Test def testEnd(): Unit = {
    assertEquals( Seq() , end)
  }

  @Test def testCons(): Unit = {
    assertEquals(              End()   ,           end )
    assertEquals(        Cel(0,End())  ,      0 :: end )
    assertEquals( Cel(0, Cel(1,End())) , 0 :: 1 :: end )
  }

  @Test def testConcat(): Unit = {
    assertEquals(                end ,        end  ++                       end   )
    assertEquals(           1 :: end ,        end  ++                 (1 :: end)  )
    assertEquals(           1 :: end ,  (1 :: end) ++                       end   )
    assertEquals(      1 :: 2 :: end ,  (1 :: end) ++                 (2 :: end)  )
    assertEquals( 1 :: 2 :: 3 :: end , ((1 :: end) ++  (2 :: end)) ++ (3 :: end)  )
    assertEquals( 1 :: 2 :: 3 :: end ,  (1 :: end) ++ ((2 :: end)  ++ (3 :: end)) )
  }

  @Test def testApply(): Unit = {
    assertEquals(             End()   , Seq()    )
    assertEquals(       Cel(1,End())  , Seq(1)   )
    assertEquals( Cel(1,Cel(2,End())) , Seq(1,2) )
  }

  @Test def testUnapplySeq(): Unit = {
    assertEquals( Some(List.empty) , Seq.unapplySeq(Seq())    )
    assertEquals( Some(List(1))    , Seq.unapplySeq(Seq(1))   )
    assertEquals( Some(List(1,2))  , Seq.unapplySeq(Seq(1,2)) )

    assertTrue( Seq()    match { case Seq()    => true ; case _ => false } )
    assertTrue( Seq(1)   match { case Seq(1)   => true ; case _ => false } )
    assertTrue( Seq(1,2) match { case Seq(1,2) => true ; case _ => false } )
  }

  val one =      1 :: end
  val two = 1 :: 2 :: end

  @Test def testFoldLeft(): Unit = {
    assertEquals( (3,"1122") , two.foldl((1,""))((c,s) => a => (c + 1, s + c + a)))
  }

  @Test def testFoldRight(): Unit = {
    assertEquals( (3,"1221") , two.foldr((1,""))(a => (c,s) => (c + 1, s + c + a)))
  }

  @Test def testMap(): Unit = {
    assertEquals( "1" :: "2" :: end , two.map(_.toString))
  }

  @Test def testBind(): Unit = {
    assertEquals( 1 :: 1 :: 2 :: 2 :: end , two.bind(a => Seq(a,a)))
  }

  @Test def testFlatMap(): Unit = {
    assertEquals( 4 :: 5 :: 5 :: 6 :: end , for {
      a <- Seq(1,2)
      b <- Seq(3,4)
    } yield (a + b))
  }

  @Test def testMkString(): Unit = {
    assertEquals( "123" , Seq(1,2,3).mkString )
    assertEquals( "[1 2 3]" , Seq(1,2,3).mkString("["," ","]") )
  }
}