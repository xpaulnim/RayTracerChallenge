package raytracer

import org.scalatest._
import Intersection.hit

class IntersectionSpec extends FlatSpec with Matchers{

  behavior of "hit"

  it should "be non empty when all intersections have a postive t" in {
    val sphere = Sphere()
    val i1 = Intersection(1, sphere)
    val i2 = Intersection(2, sphere)

    val intersections = hit(Seq(i2, i1))

    intersections should equal(Some(i1))
  }

  it should "be non empty when some intersections have negative t" in {
    val sphere = Sphere()
    val i1 = Intersection(-1, sphere)
    val i2 = Intersection(1, sphere)

    val intersection = hit(Seq(i2, i1))

    intersection should equal(Some(i2))
  }

  it should "return nothing when all intersections have negative t" in {
    val sphere = Sphere()
    val i1 = Intersection(-2, sphere)
    val i2 = Intersection(-1, sphere)

    val intersection = hit(Seq(i2, i1))

    intersection should equal(None)
  }

  it should "be the lowest non-negative intersection" in {
    val sphere = Sphere()
    val i1 = Intersection(5, sphere)
    val i2 = Intersection(7, sphere)
    val i3 = Intersection(-3, sphere)
    val i4 = Intersection(2, sphere)

    val intersection = hit(Seq(i1, i2, i3, i4))

    intersection should equal(Some(i4))
  }
}
