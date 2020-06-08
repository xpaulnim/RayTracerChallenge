package raytracer

import scala.language.implicitConversions

object DoubleEnhancements {
  val EPSILON = 0.00001

  implicit class DoubleExtensions(val self: Double) extends AnyVal {
    def ~=(other: Double, epsilon: Double = EPSILON): Boolean = math.abs(self - other) < epsilon
  }
}