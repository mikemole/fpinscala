package fpinscala.errorhandling

import org.junit.Test
import org.junit.Assert._

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter
import fpinscala.errorhandling._

class EitherTest {

  @Test
  def testMapLeft() {
    val l = Left("error")
    assertEquals(l, l.map { x => "not error" })
  }
  
  @Test
  def testMapRight() {
    val r = Right("I am")
    val expected = Right("I am right")
    assertEquals(expected, r.map { x => x + " right" })
  }
  
}