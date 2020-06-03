package raytracer

import java.nio.file.{FileSystems, Files, StandardOpenOption}

object Utils {
  val MAX_COLORS = 255

  def scaleToColor(color: Tuple): (Int, Int, Int) = {
    (
      math.ceil(scale(color.x, 0, 1, 0, 255)).toInt,
      math.ceil(scale(color.y, 0, 1, 0, 255)).toInt,
      math.ceil(scale(color.z, 0, 1, 0, 255)).toInt
    )
  }

  def scale(from: Double, fromMin: Double, fromMax: Double, toMin: Double, toMax: Double): Double = {
    if (from < fromMin) {
      return toMin
    } else if (from > fromMax) {
      return toMax
    }

    val fromAbs = from - fromMin
    val fromMaxAbs = fromMax - fromMin

    val normal = fromAbs / fromMaxAbs

    val toMaxAbs = toMax - toMin
    val toAbs = toMaxAbs * normal

    toAbs + toMin
  }

  def clamp(num: Double, min: Int, max: Double): Double = {
    if (num < min)
      return min

    if (num > max)
      return max

    num
  }

  def writeStrToFile(str: String, basePath: String = System.getProperty("user.dir")): Unit = {
    val path = FileSystems.getDefault.getPath(basePath, "render.ppm")

    Files.deleteIfExists(path)
    Files.write(path, str.getBytes(), StandardOpenOption.CREATE_NEW)
  }
}
