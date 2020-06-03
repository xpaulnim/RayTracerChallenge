package raytracer

import raytracer.Utils.{MAX_COLORS, scaleToColor}

case class Canvas(width: Int, height: Int, color:Tuple) {
  val pixels: Array[Array[Tuple]] = Array.ofDim[Tuple](height, width)

  for (row <- 0 until height)
    for (col <- 0 until width)
      pixels(row)(col) = color

  def writePixel(x:Double, y: Double, color: Tuple): Unit = {
    writePixel(x.toInt, y.toInt, color)
  }

  def writePixel(x: Int, y: Int, color: Tuple) {
    if(x > 0 && x < width && y > 0 && y < height) {
      pixels(y)(x) = color
    }
  }

  def ppmHeader(): String = {
    s"""P3
		 |$width $height
		 |$MAX_COLORS""".stripMargin
  }

  def toPPM(): String = {
    val MAX_LINE_WIDTH = 70

    val ppmStr = new StringBuilder()
    ppmStr ++= ppmHeader()
    ppmStr ++= "\n"

    for (row <- 0 until height) {

      val line  = pixels(row).flatMap (col => {
        val color = scaleToColor(col)

        val red = s"${color._1}"
        val green = s"${color._2}"
        val blue = s"${color._3}"

        Seq(red, green, blue)
      })

      // add new line at end of line or at 70 characters
      val lineStr = new StringBuilder()
      for(i <- line.indices) {
        lineStr ++= line(i)
        lineStr ++= " "

        if(i != line.length - 1 && (lineStr ++ line(i+1)).length > MAX_LINE_WIDTH) {
          lineStr.deleteCharAt(lineStr.length()-1)
          lineStr ++= "\n"
          ppmStr ++= lineStr
          lineStr.clear()
        }
      }

      ppmStr ++= lineStr.toString().trim
      ppmStr ++= "\n"
    }

    ppmStr.mkString
  }
}

object Canvas {
  def apply(width: Int, height: Int, color: Tuple): Canvas = new Canvas(width, height, color)

  def apply(width: Int, height: Int): Canvas = new Canvas(width, height, Color(0, 0, 0))
}