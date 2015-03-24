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
  def testSetHeadOnListOfOne() {
    val l = List(2)
    assertEquals(List(1), List.setHead(l, 1))
  }
  
  @Test
  def testSetHeadOnNilNeg() {
    control.Exception.ignoring(classOf[UnsupportedOperationException]) {
      List.setHead(Nil, 1)
      fail("Expected UnsupportedOperationException")
    } 
  }
  
  @Test
  def testSetNilHead() {
    // TODO: Should this throw or succeed?
    assertEquals(List(Nil), List.setHead(List(1), Nil))
  }
  
  @Test
  def testDrop() {
    val l = List(1,2,3,4)
    val d = List.drop(l, 2)
    assertEquals(List(3,4), d)
  }
  
  @Test
  def testDropTooMany() {
    val l = List(1,2)
    assertEquals(List(), List.drop(l, 3))
  }
  
  @Test
  def testDropWhile() {
    val l = List(1,2,7,2,5)
    val dw = List.dropWhile(l, (x: Int) => (x <= 3))
    assertEquals(List(7,2,5), dw)
  }
  
  @Test
  def testInit() {
    val l = List(1,2,3,4)
    val expected = List(1,2,3)
    assertEquals(expected, List.init(l))
  }
  
  @Test
  def testInitListOfOne() {
    val l = List(1)
    val expected = List()
    assertEquals(expected, List.init(l))
  }
  
  @Test
  def testLengthViaFoldRight() {
    assertEquals(3, List.length(List(1,4,9)))
  }
  
  @Test
  def testLengthViaFoldLeft() {
    val l = List(1,2,3)
    val len = List.foldLeft(l, 0)((acc, _) => acc + 1)
    assertEquals(3, len)
  }
  
  @Test
  def testSum3() {
    val l = List(1,2,3)
    assertEquals(6, List.sum3(l))
  }
  
  @Test
  def testProduct3() {
    val l = List(1.0, 2.0, 5.0)
    assertEquals(10.0, List.product3(l), 0)
  }
  
  @Test
  def testLength3() {
    val l = List(1,2,3)
    assertEquals(3, List.length3(l))
  }
  
  @Test
  def testReverse() {
    val l = List(1,2,3)
    val r = List(3,2,1)
    assertEquals(r, List.reverse(l))
  }
  
  @Test
  def testAppendInTermsOfFold() {
    val a1 = List(1,2)
    val a2 = List(3,4)
    val l = List.append2(a1, a2)
    assertEquals(List(1,2,3,4), l)
  }
  
  @Test
  def testConcatListOfLists() {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    val l3 = List(7,8,9)
    val lol = List(l1, l2, l3)
    
    val expected = List(1,2,3,4,5,6,7,8,9)
    val cl = List.concat(lol)
    assertEquals(expected, cl)
  }
  
  @Test
  def testPlusOne() {
    val l1 = List(4,6,8)
    val expected = List(5,7,9)
    assertEquals(expected, List.plusOne(l1))
  }
  
  @Test
  def testListDoublesToStrings() {
    val ld = List(1.0, 2.7, 3.0)
    val ls = List("1.0","2.7","3.0")
    assertEquals(ls, List.doublesToStrings(ld))
  }
  
  @Test
  def testMap() {
    val ld = List(1.0, 2.7, 3.0)
    val ls = List("1.0","2.7","3.0")
    assertEquals(ls, List.map(ld)(d => d.toString()))
  }
  
  @Test
  def testFilter() {
    val l = List(1,2,3,4,7,8,10)
    val expected = List(2,4,8,10)
    val filtered = List.filter(l)(x => x % 2 == 0)
    assertEquals(expected, filtered)
  }
  
  @Test
  def testFlatMap() {
    val expected = List(1,1,2,2,3,3)
    val l = List.flatMap(List(1,2,3))(i => List(i,i))
    assertEquals(expected, l)
  }
  
  @Test
  def testFilterViaFlatMap() {
    val l = List(1,2,3,4,7,8,10)
    val expected = List(2,4,8,10)
    val filtered = List.filter2(l)(x => x % 2 == 0)
    assertEquals(expected, filtered)
  }
  
  // 3.22
  @Test
  def testZipWithAddition() {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    val expected = List(5,7,9)
    assertEquals(expected, List.zipWithAddition(l1, l2))
  }
  
  // 3.23
  @Test
  def testZipWith() {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    val expected = List(5,7,9)
    val actual = List.zipWith(l1, l2)(_ + _)
  }
}
