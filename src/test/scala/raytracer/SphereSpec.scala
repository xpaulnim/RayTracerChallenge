package raytracer

import org.scalatest.{FlatSpec, Matchers}
import DoubleEnhancements._

class SphereSpec extends FlatSpec with Matchers {

  behavior of "intersects"

  it should "intersect a sphere at two points" in {
    val ray = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val sphere = Sphere()

    val xs = sphere.intersect(ray)

    xs.length should equal(2)
    xs(0).t should equal(4.0 +- EPSILON)
    xs(1).t should equal(6.0 +- EPSILON)
  }

  it should "intersect a sphere at a tangent" in {
    val ray = Ray(Point(0, 1, -5), Vector(0, 0, 1))
    val sphere = Sphere()

    val xs = sphere.intersect(ray)

    xs.length should equal(2)
    xs(0).t should equal(5.0 +- EPSILON)
    xs(1).t should equal(5.0 +- EPSILON)
  }

  it should "miss a sphere" in {
    val ray = Ray(Point(0, 2, -5), Vector(0, 0, 1))
    val sphere = Sphere()

    val xs = sphere.intersect(ray)

    xs.length should equal(0)
  }

  it should "intersect for a ray originating inside a sphere" in {
    val ray = Ray(Point(0, 0, 0), Vector(0, 0, 1))
    val sphere = Sphere()

    val xs = sphere.intersect(ray)

    xs.length should equal(2)
    xs(0).t should equal(-1.0 +- EPSILON)
    xs(1).t should equal(1.0 +- EPSILON)
  }

  it should "intersect for a ray behind the sphere" in {
    val ray = Ray(Point(0, 0, 5), Vector(0, 0, 1))
    val sphere = Sphere()

    val xs = sphere.intersect(ray)

    xs.length should equal(2)
    xs(0).t should equal(-6.0 +- EPSILON)
    xs(1).t should equal(-4.0 +- EPSILON)
  }

  it should "set the object on the intersection" in {
    val ray = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val sphere = Sphere()

    val xs = sphere.intersect(ray)

    xs.size should equal(2)
    xs(0).obj should equal(sphere)
    xs(0).obj should equal(sphere)
  }
}
