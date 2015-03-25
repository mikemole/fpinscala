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
    
    val size = Tree.size(root)
    assertEquals(5, size)
  }

  @Test
  def testSizeNeg() {
    assertEquals(Tree.size(null), 0)
  }
}
