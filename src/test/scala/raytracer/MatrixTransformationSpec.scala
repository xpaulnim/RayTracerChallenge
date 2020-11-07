package raytracer

import org.scalatest.{FlatSpec, Matchers}
import MatrixTransformation._

import scala.math.Pi

class MatrixTransformationSpec extends FlatSpec with Matchers {

  behavior of "MatrixTransformation"

  it should "multiply by a translation matrix" in {
    val transform = translation(5, -3, 2)
    val point = Point(-3, 4, 5)

    assert((transform * point) == Point(2, 1, 7))
  }

  it should "multiply by the inverse of a translation matrix" in {
    val transform = translation(5, -3, 2)
    val v = Vector(-3, 4, 5)

    assert((transform * v) == v)
  }

  it should "scale a matrix applied to a point" in {
    val transform = scaling(2, 3, 4)
    val p = Point(-4, 6, 8)

    assert((transform * p) == Point(-8, 18, 32))
  }

  it should "scale a matrix applied to a vector" in {
    val transform = scaling(2, 3, 4)
    val v = Vector(-4, 6, 8)

    assert((transform * v) == Vector(-8, 18, 32))
  }

  it should "multiply by the inverse of a scaling matrix" in {
    val transform = scaling(2, 3, 4)
    val v = Vector(-4, 6, 8)

    assert((transform.inverse() * v) == Vector(-2, 2, 2))
  }

  it should "reflection is scaling by a negative value" in {
    val transform = scaling(-1, 1, 1)
    val p = Point(2, 3, 4)

    assert((transform * p) == Point(-2, 3, 4))
  }

  it should "rotate a point around the x axis" in {
    val p = Point(0, 1, 0)

    val halfQuarter = rotationX(Pi / 4)
    val fullQuarter = rotationX(Pi / 2)

    println(halfQuarter)
    println()
    println(fullQuarter)

    val tuple = Point(0, math.sqrt(2) / 2, math.sqrt(2) / 2)

    println()
    println(tuple)

    val tuple1 = halfQuarter * p
    println()
    println(tuple1)

    assert(tuple1 == tuple)

    assert(fullQuarter * p == Point(0, 0, 1))
  }

  it should "the inverse of an x rotation rotates in the opposite direction" in {
    val p = Point(0, 1, 0)
    val halfQuarter = rotationX(Pi / 4)

    assert((halfQuarter.inverse() * p) == Point(0, math.sqrt(2) / 2, -math.sqrt(2) / 2))
  }

  it should "rotate a point around the y axis" in {
    val p = Point(0, 0, 1)

    val halfQuarter = rotationY(Pi / 4)
    val fullQuarter = rotationY(Pi / 2)

    ((halfQuarter * p) == Point(math.sqrt(2) / 2, 0, math.sqrt(2) / 2)) should be(true)
    ((fullQuarter * p) == Point(1, 0, 0)) should be(true)
  }

  it should "rotate a point around the z axis" in {
    val p = Point(0, 1, 0)

    val halfQuarter = rotationZ(Pi / 4)
    val fullQuarter = rotationZ(Pi / 2)

    assert((halfQuarter * p) == Point(-math.sqrt(2) / 2, math.sqrt(2) / 2, 0))
    assert((fullQuarter * p) == Point(-1, 0, 0))
  }

  it should "perform shearing transformation to move x in proportion to y" in {
    val transform = shearing(1, 0, 0, 0, 0, 0)
    val p = Point(2, 3, 4)

    assert((transform * p) == Point(5, 3, 4))
  }

  it should "shearing transformation moves x in proportion to z" in {
    val transform = shearing(0, 1, 0, 0, 0, 0)
    val p = Point(2, 3, 4)

    assert((transform * p) == Point(6, 3, 4))
  }

  it should "shearing transformation moves y in proportion to x" in {
    val transform = shearing(0, 0, 1, 0, 0, 0)
    val p = Point(2, 3, 4)

    assert((transform * p) == Point(2, 5, 4))
  }

  it should "shearing transformation moves y in proportion to z" in {
    val transform = shearing(0, 0, 0, 1, 0, 0)
    val p = Point(2, 3, 4)

    assert((transform * p) == Point(2, 7, 4))
  }

  it should "shearing transformation moves z in proportion to x" in {
    val transform = shearing(0, 0, 0, 0, 1, 0)
    val p = Point(2, 3, 4)

    assert((transform * p) == Point(2, 3, 6))
  }

  it should "shearing transformation moves z in proportion to y" in {
    val transform = shearing(0, 0, 0, 0, 0, 1)
    val p = Point(2, 3, 4)

    assert((transform * p) == Point(2, 3, 7))
  }

  it should "apply individual transformations in sequence" in {
    val p = Point(1, 0, 1)
    val A = rotationX(Pi / 2)
    val B = scaling(5, 5, 5)
    val C = translation(10, 5, 7)

    val p2 = A * p
    assert(p2 == Point(1, -1, 0))

    val p3 = B * p2
    assert(p3 == Point(5, -5, 0))

    val p4 = C * p3
    assert(p4 == Point(15, 0, 7))
  }

  it should "apply chained transformations in reverse order" in {
    val p = Point(1, 0, 1)
    val A = rotationX(Pi / 2)
    val B = scaling(5, 5, 5)
    val C = translation(10, 5, 7)
    val T = C * B * A

    assert((T * p) == Point(15, 0, 7))
  }

}
