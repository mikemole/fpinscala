package fpinscala.datastructures;


import org.junit.Test
import org.junit.Assert._
import scala.util.control

class ListTest {

  @Test
  def test_3_1() {
    assertEquals(3, List.x)
  }

  @Test
  def testTail() {
    val l = List(1,2,3)
    assertEquals(List(2,3), List.tail(l))
  }
  
  @Test
  def testTailWithNil() {
    control.Exception.ignoring(classOf[UnsupportedOperationException]) {
      List.tail(Nil)
      fail("Expected UnsupportedOperationException")
    } 
  }
  
  @Test
  def testSetHead() {
    val l = List(5,2,3)
    val nh = List.setHead(l, 1)
    assertEquals(List(1,2,3), nh)
  }
  
  @Test
  def setHeadOnListOfOne() {
    val l = List(2)
    assertEquals(List(1), List.setHead(l, 1))
  }
  
  @Test
  def setHeadOnNilNeg() {
    control.Exception.ignoring(classOf[UnsupportedOperationException]) {
      List.setHead(Nil, 1)
      fail("Expected UnsupportedOperationException")
    } 
  }
  
  @Test
  def setNilHead() {
    // TODO: Should this throw or succeed?
    assertEquals(List(Nil), List.setHead(List(1), Nil))
  }
}
