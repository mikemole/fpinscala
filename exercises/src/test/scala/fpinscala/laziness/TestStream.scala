package fpinscala.laziness

import org.junit.Test
import org.junit.Assert._

import fpinscala.laziness._

class TestStream {

  @Test
  def testToList() {
    val s = Stream.cons(1, Stream.cons(2, Empty))
    val ex = List(1, 2)
    assertEquals(ex, s.toList)
  }
  
  @Test
  def testTake() {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    val ex = Stream(1, 2).toList
    assertEquals(ex, s.take(2).toList)
  }
  
  @Test
  def testDrop() {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    val ex = Stream(3).toList
    assertEquals(ex, s.drop(2).toList)
  }
  
  @Test
  def testTakeWhile() {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    val ex = Stream(1, 2).toList
    assertEquals(ex, s.takeWhile(x => x < 3).toList)
  }
  
  @Test
  def testForAll() {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    assertTrue(s.forAll { x => x < 4 })
  }
  
  @Test
  def testForAllNeg() {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    assertFalse(s.forAll { x => x < 3 })
  }
  
  @Test
  def testTakeWhileViaFoldRight() {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    val ex = Stream(1, 2).toList
    assertEquals(ex, s.takeWhileViaFoldRight(x => x < 3).toList)
  }
  
  @Test
  def testHeadOptionNonEmpty() {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    val ho = s.headOption
    assertEquals(1, ho.get)
  }
  
  @Test
  def testHeadOptionEmpty() {
    assertEquals(None, Empty.headOption)
  }
  
  @Test
  def testMap() {
    val s = Stream(1, 2, 3)
    val ex = Stream(2, 4, 6)
    assertEquals(ex.toList, s.map(x => x * 2).toList)
  }
  
  @Test
  def testFilter() {
    val s = Stream(1, 2, 3)
    val ex = Stream(2)
    assertEquals(ex.toList, s.filter(x => x % 2 == 0).toList)
  }
  
  @Test
  def testAppend() {
    val s = Stream(1, 2) append Stream(3, 4)
    val ex = Stream(1, 2, 3, 4)
    assertEquals(ex.toList, s.toList)
  }
  
  @Test
  def testFlatMap() {
    val s = Stream(Stream(1,2), Stream(3,4))
    val ex = Stream(2,4,6,8)
    assertEquals(ex.toList, s.flatMap(x => x.map(_ * 2)).toList)
  }
  
  @Test
  def testConstant() {
    val ex = List("b", "b", "b")
    val bs = Stream.constant("b": String)
    assertEquals(ex, bs.take(3).toList)
  }
}