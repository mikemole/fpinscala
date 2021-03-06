package fpinscala.errorhandling

import org.junit.Test
import org.junit.Assert._

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import fpinscala.errorhandling._

class OptionTest {

  @Test
  def testMapOfNone() {
    assertEquals(None, None.map(x => 1))
  }
  
  @Test
  def testMapOfSome() {
    val s = Some(1)
    assertEquals(Some(2), s.map(x => x + 1))
  }
  
  @Test
  def testGetOrElseOfNone() {
    assertEquals(4, None.getOrElse(4))
  }
  
  @Test
  def testGetOrElse() {
    assertEquals(2, Some(2).getOrElse(4))
  }
  
  @Test
  def testFlatMapOfNone() {
    assertEquals(None, None.flatMap { x => Some(1) })
  }
  
  @Test
  def testFlatMapOfSomeWithoutFail() {
    assertEquals(Some(2), Some(1).flatMap(x => Some(2*x)))
  }
  
  @Test
  def testFlatMapOfSomeWithFail() {
    assertEquals(None, Some(1).flatMap(_ => None))
  }
  
  @Test
  def testOrElseOfNone() {
    assertEquals(Some(1), None.orElse(Some(1)))
  }
  
  @Test
  def testOrElse() {
    assertEquals(Some(2), Some(2).orElse(Some(1)))
  }
  
  @Test
  def testFilterOfNone() {
    assertEquals(None, None.filter(x => x == 2))
  }
  
  @Test
  def testFilterToSome() {
    assertEquals(Some(2), Some(2).filter(x => x == 2))
  }
  
  @Test
  def testFilterToNone() {
    assertEquals(None, Some(2).filter(x => x == 3))
  }
  
  @Test
  def testVariance() {
    val xs = Seq(1.0, 2.0, 3.0, 4.0)
    assertEquals(Some(1.25), Option.variance(xs))
  }
  
  @Test
  def testMap2() {
    val a = Some(1)
    val b = Some(2)
    val ex = Option.map2(a, b)((a,b) => a + b)
    assertEquals(Some(3), ex)
  }
  
  @Test
  def testMap2Neg01() {
    val a = Some(1)
    val b = None:Option[Int]
    val ex = Option.map2(a, b)((a,b) => a + b)
    assertEquals(None, ex)
  }
  
  @Test
  def testMap2Neg02() {
    val a = None:Option[Int]
    val b = Some(1)
    val ex = Option.map2(a, b)((a,b) => a + b)
    assertEquals(None, ex)
  }
  
  @Test
  def testSequence() {
    val a = List(Some(1), Some(2), Some(3))
    val ex = Some(List(1,2,3))
    assertEquals(ex, Option.sequence(a))
  }
  
  @Test
  def testTraverse() {
    val a = List("1","2","3")
    val ex = Some(List(1,2,3))
    assertEquals(ex, Option.traverse(a)(i => Option.Try(i.toInt)))
  }
}