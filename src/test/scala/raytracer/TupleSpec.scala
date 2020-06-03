package raytracer

import org.scalatest._

class TupleSpec extends FlatSpec with Matchers {

  "MultiplyTupleByTuple" should "multiply two tuples" in {
    val c1 = Color(1, 0.2, 0.4)
    val c2 = Color(0.9, 1, 0.1)

    ((c1 * c2) == Color(0.9, 0.2, 0.04)) should be(true)
  }

  "Color" should "create a tuple of Color" in {
    val c = Color(-0.5, 0.4, 1.7)

    c.x should be(-0.5)
    c.y should be(0.4)
    c.z should be(1.7)
  }

  "CrossProduct" should "compute the cross product of vectors" in {
    val v1 = Vector(1, 2, 3)
    val v2 = Vector(2, 3, 4)

    v1.cross(v2) should equal(Vector(-1, 2, -1))
    v2.cross(v1) should equal(Vector(1, -2, 1))
  }

  "DotProductTuples" should "compute the dot product of two vectors" in {
    val v1 = Vector(1, 2, 3)
    val v2 = Vector(2, 3, 4)

    v1.dot(v2) should be (20)
  }

  "NormaliseVector" should "normalise vectors" in {
    val v1 = Vector(4, 0, 0)
    assert(v1.normalise == Vector(1, 0, 0))

    val v2 = Vector(1, 2, 3)
    val magnitude = math.sqrt(14)
    assert(v2.normalise == Vector(1/magnitude, 2/magnitude, 3/magnitude))

    // magnitude of normalised vector
    val v3 = Vector(1, 2, 3)
    v3.normalise().magnitude should be(1)
  }

  "Magnitude" should "compute the magnitude of a vector" in {
    val v1 = Vector(1, 0, 0)
    v1.magnitude should equal(1)

    val v2 = Vector(0, 1, 0)
    v2.magnitude should equal(1)

    val v3 = Vector(0, 0, 1)
    v3.magnitude should equal(1)

    val v4 = Vector(1, 2, 3)
    v4.magnitude should equal(math.sqrt(14))

    val v5 = Vector(-1, -2, -3)
    v5.magnitude should equal (math.sqrt(14))
  }

  "DivideTupleByScalar" should "divide a tuple by a scalar" in {
    val a = Tuple(1, -2, 3, -4)
    val r1 = a / 2

    r1 should equal(Tuple(0.5, -1, 1.5, -2))
  }

  "MultiplyTupleByScalar" should "multiply a tuple by a scalar" in {
    val a = Tuple(1, -2, 3, -4)
    val r1 = a * 3.5
    r1 should be (Tuple(3.5, -7, 10.5, -14))

    val r2 = a * 0.5
    r2 should be (Tuple(0.5, -1, 1.5, -2))

    // Color
    val c = Color(0.2, 0.3, 0.4)
    (c * 2) === Color(0.4, 0.6, 0.8) should be (true)
  }

  "NegateTuples" should "negate tuples" in {
    val zero = Vector(0, 0, 0)
    val v = Vector(1, -2, 3)
    val r1 = (zero - v)
    r1 should equal(Vector(-1, 2, -3))

    val a = Tuple(1, -2, 3, -4)
    -a should be (Tuple(-1, 2, -3, 4))
  }

  "SubtractTuples" should "subtract tuples" in {
    val p1 = Point(3, 2, 1)
    val p2 = Point(5, 6, 7)
    val r1 = p1 - p2
    r1 should be(Vector(-2, -4, -6))

    val p = Point(3, 2, 1)
    val v = Vector(5, 6, 7)
    val r2 = p - v
    r2 should be (Point(-2, -4, -6))

    val v1 = Vector(3, 2, 1)
    val v2 = Vector(5, 6, 7)
    val r3 = v1 - v2
    r3 should be (Vector(-2, -4, -6))

    // Color
    val c1 = Color(0.9, 0.6, 0.75)
    val c2 = Color(0.7, 0.1, 0.25)
    val r4 = c1 - c2
    r4 === Color(0.2, 0.5, 0.5) should be(true)
  }

  "AddTuples" should "add Tuples" in {
    val a = Tuple(3, -2, 5, 1)
    val b = Tuple(-2, 3, 1, 0)
    val r1 = a + b
    r1 should equal(Point(1, 1, 6))

    // Color
    val c1 = Color(0.9, 0.6, 0.75)
    val c2 = Color(0.7, 0.1, 0.25)
    val r2 = c1 + c2
    r2 === Color(1.6, 0.7, 1.0)
  }


}