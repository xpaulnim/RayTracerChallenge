package raytracer

import org.scalatest.{FlatSpec, Matchers}
import raytracer.DoubleEnhancements._

class DoubleEnhancementsSpec extends FlatSpec with Matchers {
  "~=" should "compare double within to within the epsilon" in {
    val a = 0.3
    val b = 0.1 + 0.2

    a should not equal(b)
    (a ~= b) should be(true)
  }
}
