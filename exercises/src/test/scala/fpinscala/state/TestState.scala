package fpinscala.state

import org.junit.Test
import org.junit.Assert._

import fpinscala.state._

class TestState {

  @Test
  def testNonNegativeInt() {
    val rng = RNG.Simple(0)
    assertTrue(rng.nextInt._1 >= 0)
  }
  
}