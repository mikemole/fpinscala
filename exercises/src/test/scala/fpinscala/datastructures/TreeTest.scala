package fpinscala.datastructures;


import org.junit.Test
import org.junit.Assert._
import scala.util.control

class TestTest {

  @Test
  def testSize() {
    val l1 = Leaf(1)
    val l2 = Leaf(2)
    val b1 = Branch(l1, l2)
    val l3 = Leaf(3)
    val root = Branch(l3, b1)
    
    val size = Tree.size(makeTree())
    assertEquals(5, size)
  }

  def makeTree(): Branch[Int] = {
    makeTree(1)
  }
  
  def makeTree(multiplier: Int): Branch[Int] = {
    val l1 = Leaf(1 * multiplier)
    val l2 = Leaf(2 * multiplier)
    val b1 = Branch(l1, l2)
    val l3 = Leaf(3 * multiplier)
    Branch(l3, b1)
  }
  
  @Test
  def testSizeNeg() {
    assertEquals(Tree.size(null), 0)
  }
  
  @Test
  def testMaximum() {
    assertEquals(3, Tree.maximum(makeTree()))
  }
  
  @Test
  def testDepth() {
    assertEquals(3, Tree.depth(makeTree()))
  }
  
  @Test
  def testMap() {
    assertEquals(makeTree(2), Tree.map(makeTree(1))(v => 2*v))
  }
  
  // 3.30
  @Test
  def testSize2() {
    assertEquals(5, Tree.size2(makeTree()))
  }
  
  @Test
  def testMaximum2() {
    assertEquals(3, Tree.maximum2(makeTree()))
  }
  
  @Test
  def testDepth2() {
    assertEquals(3, Tree.depth2(makeTree()))
  }
  
  @Test
  def testMap2() {
    assertEquals(makeTree(2), Tree.map2(makeTree(1))(v => 2*v))
  }
}
