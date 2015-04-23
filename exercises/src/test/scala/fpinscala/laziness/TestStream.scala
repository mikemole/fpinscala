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
  
  @Test
  def testFrom() {
    val ex = List(3,4,5,6)
    val v = Stream.from(3).take(4).toList
    assertEquals(ex, v)
  }
  
  @Test
  def testFibs() {
    val ex = List(0,1,1,2,3,5,8)
    val v = Stream.fibs.take(7).toList
    assertEquals(ex, v)
  }
  
  @Test
  def testUnfold() {
    val ones = Stream.unfold(Stream.empty[Int])(s => Some(1, s))
    val ex = List(1, 1, 1)
    assertEquals(ex, ones.take(3).toList)
  }
  
  @Test
  def testOnesViaUnfold() {
    val ex = List(1, 1, 1)
    val v = Stream.onesViaUnfold.take(3).toList
    assertEquals(ex, v)
  }
  
  @Test
  def testFromViaUnfold() {
    val ex = List(3,4,5,6)
    val v = Stream.fromViaUnfold(3).take(4).toList
    assertEquals(ex, v)
  }
  
  @Test
  def testConstantViaUnfold() {
    val ex = List("b", "b", "b")
    val bs = Stream.constantViaUnfold("b": String)
    assertEquals(ex, bs.take(3).toList)
  }
  
  @Test
  def testFibsViaUnfold() {
    val ex = List(0,1,1,2,3,5,8)
    val v = Stream.fibsViaUnfold.take(7).toList
    assertEquals(ex, v)
  }
  
  @Test
  def testMapViaUnfold() {
    val s = Stream(1, 2, 3)
    val ex = Stream(2, 4, 6)
    assertEquals(ex.toList, s.mapViaUnfold(x => x * 2).toList)
  }
  
  @Test
  def testTakeViaUnfold() {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty)))
    val ex = Stream(1, 2).toList
    assertEquals(ex, s.takeViaUnfold(2).toList)
  }
  
  @Test
  def testTakeWhileViaUnfold() {
    val s = Stream(1,2,3)
    val ex = Stream(1,2).toList
    assertEquals(ex, s.takeWhileViaUnfold(x => x < 3).toList)
  }
  
  @Test
  def testZipWith() {
    val s1 = Stream(1,2,3)
    val s2 = Stream(4,5,6)
    val expected = List(5,7,9)
    val actual = s1.zipWith(s2)(_ + _).toList
    assertEquals(expected, actual)
  }
  
}