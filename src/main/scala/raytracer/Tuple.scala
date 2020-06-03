package raytracer

import raytracer.DoubleEnhancements._

case class Tuple(x: Double, y: Double, z: Double, w: Double) {
  val isPoint: Boolean = w == 1.0
  val isVector: Boolean = w == 0.0

  // Adding a point and a point doesn't make sense. w+w for Point gives 2 which is neither point nor vector
  def +(that: Tuple): Tuple = {
    val x = this.x + that.x
    val y = this.y + that.y
    val z = this.z + that.z
    val w = this.w + that.w

    Tuple(x, y, z, w)
  }

  def -(that: Tuple): Tuple = {
    val x = this.x - that.x
    val y = this.y - that.y
    val z = this.z - that.z
    val w = this.w - that.w

    Tuple(x, y, z, w)
  }

  // negate
  def unary_-(): Tuple = {
    Vector(0, 0, 0) - this
  }

  def *(scalar: Double): Tuple = {
    val x = this.x * scalar
    val y = this.y * scalar
    val z = this.z * scalar
    val w = this.w * scalar

    Tuple(x, y, z, w)
  }

  def /(scalar: Double): Tuple = {
    val x = this.x / scalar
    val y = this.y / scalar
    val z = this.z / scalar
    val w = this.w / scalar

    Tuple(x, y, z, w)
  }

  // vectors with magnitude 1 are unit vectors
  def magnitude(): Double = {
    val x = math.pow(this.x, 2)
    val y = math.pow(this.y, 2)
    val z = math.pow(this.z, 2)
    val w = math.pow(this.w, 2)

    math.sqrt(x + y + z + w)
  }

  def normalise(): Tuple = {
    val x = this.x / this.magnitude
    val y = this.y / this.magnitude
    val z = this.z / this.magnitude
    val w = this.w / this.magnitude

    Tuple(x, y, z, w)
  }

  def dot(that: Tuple): Double = {
    this.x * that.x +
      this.y * that.y +
      this.z * that.z +
      this.w * that.w
  }

  def cross(that: Tuple): Tuple = {
    val x = this.y * that.z - this.z * that.y
    val y = this.z * that.x - this.x * that.z
    val z = this.x * that.y - this.y * that.x

    Vector(x, y, z)
  }

  def ==(that: Tuple): Boolean = {
    (this.x ~= that.x) &&
      (this.y ~= that.y) &&
      (this.z ~= that.z) &&
      (this.w ~= that.w)
  }

  def ===(that: Tuple): Boolean = {
    (this.x ~= that.x) &&
      (this.y ~= that.y) &&
      (this.z ~= that.z)
  }

  def *(that: Tuple): Tuple = {
    // hadamard product - useful for blending colors
    val x = this.x * that.x
    val y = this.y * that.y
    val z = this.z * that.z

    Color(x, y, z)
  }
}

object Point {
  def apply(x: Double, y: Double, z: Double): Tuple = Tuple(x, y, z, 1.0)
}

object Vector {
  def apply(x: Double, y: Double, z: Double): Tuple = Tuple(x, y, z, 0.0)
}

object Color {
  def apply(red: Double, green: Double, blue: Double): Tuple = Tuple(red, green, blue, -1.0)
}