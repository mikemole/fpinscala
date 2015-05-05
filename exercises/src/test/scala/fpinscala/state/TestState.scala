package fpinscala.state

import org.junit.Test
import org.junit.Assert._

import fpinscala.state._

class TestState {

  @Test
  def testNonNegativeInt() {
    val rng = RNG.Simple(0)
    assertTrue(RNG.nonNegativeInt(rng)._1 >= 0)
  }
  
  @Test
  def testDouble() {
    val rng = RNG.Simple(0)
    val dbl = RNG.double(rng)._1
    assertTrue(dbl >=0 && dbl < 1)
  }
}