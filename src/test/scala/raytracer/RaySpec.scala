package raytracer

import org.scalatest._
import DoubleEnhancements._

class RaySpec extends FlatSpec with Matchers {
  behavior of "Ray"

  it should "compute a point from a distance" in {
    val r = Ray(Point(2, 3, 4), Vector(1, 0, 0))

    assert(r.position(0) == Point(2, 3, 4))
    assert(r.position(1) == Point(3, 3, 4))
    assert(r.position(-1) == Point(1, 3, 4))
    assert(r.position(2.5) == Point(4.5, 3, 4))
  }

}