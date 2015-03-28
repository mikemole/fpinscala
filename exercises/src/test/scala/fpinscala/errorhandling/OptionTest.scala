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
}