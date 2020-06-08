package raytracer

import org.scalatest.{FlatSpec, Matchers}

class CanvasSpec extends FlatSpec with Matchers {
  ignore should "ensure PPm files are terminated by a new line character" in {
    val canvas = Canvas(5, 3)
    canvas.toPPM() should endWith("\n")
  }

  ignore should "split long lines in PPM output to a max of 70 lines" in {
    val canvas =  Canvas(10, 2, Color(1, 0.8, 0.6))

    val expectedPPMStr = """P3
     |10 2
     |255
     |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204​
     |153 255 204 153 255 204 153 255 204 153 255 204 153​
     |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204​
     |153 255 204 153 255 204 153 255 204 153 255 204 153​
     |""".stripMargin

    canvas.toPPM() should equal(expectedPPMStr)
  }

  ignore should "write pixels to the canvas" in {
    val canvas = Canvas(5, 3)
    val c1 = Color(1.5, 0, 0)
    val c2 = Color(0, 0.5, 0)
    val c3 = Color(-0.5, 0, 1)
    canvas.writePixel(0, 0, c1)
    canvas.writePixel(2, 1, c2)
    canvas.writePixel(4, 2, c3)

    val expectedPPMStr = """P3
      |5 3
      |255
      |255 0 0 0 0 0 0 0 0 0 0 0 0 0 0​
      |​0 0 0 0 0 0 0 128 0 0 0 0 0 0 0​
      |​0 0 0 0 0 0 0 0 0 0 0 0 0 0 255
      |​""".stripMargin

    canvas.toPPM() should equal(expectedPPMStr)
  }

  ignore should "construct the PPM header" in {
    val c = Canvas(5, 3)
    val expectedPPMStr = """P3
		|5 3
		|255""".stripMargin

    c.ppmHeader() should be (expectedPPMStr)
  }

  "WritePixelsToCanvas" should "write pixel data to a canvas" in {
    val c = Canvas(10, 20)
    val red = Color(1, 0, 0)
    c.writePixel(2, 3, red)

    c.pixels(3)(2) should be (red)
  }

  "CreateCanvas" should "create a canvas with all pixels initialized to black" in {
    val canvas = Canvas(10, 20)
    assert(canvas.width == 10)
    assert(canvas.height == 20)

    for (row <- canvas.pixels.indices)
      for (col <- canvas.pixels(row).indices)
        canvas.pixels(row)(col) should be  (Color(0, 0, 0))
  }
}
