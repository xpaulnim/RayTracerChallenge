package raytracer

import org.scalatest.{FlatSpec, Matchers}
import raytracer.Utils.scale

class UtilsSpec extends FlatSpec with Matchers {
  "scale" should "scale a double to within a given range" in {
    scale(1.5, 0, 1, 0, 255) should equal(255)

    scale(1, 0, 1, 0, 255) should equal(255)

    scale(0.8, 0, 1, 0, 255) should equal(204)

    scale(0.6, 0, 1, 0, 255) should equal(153)

    scale(0.5, 0, 1, 0, 255) should equal(127.5)

    scale(0.0, 0, 1, 0, 255) should equal(0)

    scale(-0.5, 0, 1, 0, 255) should equal(0)
  }
}
