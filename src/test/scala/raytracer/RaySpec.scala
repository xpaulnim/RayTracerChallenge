package raytracer

import org.scalatest._
import DoubleEnhancements._
import raytracer.MatrixTransformation.{scaling, translation}

class RaySpec extends FlatSpec with Matchers {
  behavior of "Ray"

  it should "create and query a ray" in {
    val origin = Point(1, 2, 3)
    val direction = Vector(4, 5, 6)

    val r = Ray(origin, direction)

    assert(r.origin == origin)
    assert(r.direction == direction)
  }


  "position" should "compute a point from a distance" in {
    val r = Ray(Point(2, 3, 4), Vector(1, 0, 0))

    assert(r.position(0) == Point(2, 3, 4))
    assert(r.position(1) == Point(3, 3, 4))
    assert(r.position(-1) == Point(1, 3, 4))
    assert(r.position(2.5) == Point(4.5, 3, 4))
  }

  behavior of "transform"

  it should "translate a ray" in {
    val r = Ray(Point(1, 2, 3), Vector(0, 1, 0))
    val m = translation(3, 4, 5)

    val r2 = r.transform(m)

    assert(r2.origin == Point(4, 6, 8))
    assert(r2.direction == Vector(0, 1, 0))
  }

  it should "scale a ray" in {
    val r = Ray(Point(1, 2, 3), Vector(0, 1, 0))
    val m = scaling(2, 3, 4)

    val r2 = r.transform(m)

    assert(r2.origin == Point(2, 6, 12))
    assert(r2.direction == Vector(0, 3, 0))
  }
}