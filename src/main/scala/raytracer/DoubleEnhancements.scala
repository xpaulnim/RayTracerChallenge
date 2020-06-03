package raytracer

import scala.language.implicitConversions

object DoubleEnhancements {
  implicit class DoubleExtensions(val self: Double) extends AnyVal {
    def ~=(other: Double, epsilon: Double = 0.00001): Boolean = math.abs(self - other) < epsilon
  }
}