package raytracer

// TODO: API could be fluent
object MatrixTransformation {

  def translation(x: Double, y: Double, z: Double): Matrix = {
    val matrix = IdentityMatrix()

    matrix(0, 3) = x
    matrix(1, 3) = y
    matrix(2, 3) = z

    matrix
  }


  def scaling (x: Double, y: Double, z: Double): Matrix = {
    val matrix = IdentityMatrix()

    matrix(0, 0) = x
    matrix(1, 1) = y
    matrix(2, 2) = z

    matrix
  }

  def rotationX (radians: Double): Matrix = {
    val matrix = IdentityMatrix()

    matrix(1, 1) = math.cos(radians)
    matrix(1, 2) = -math.sin(radians)
    matrix(2, 1) = math.sin(radians)
    matrix(2, 2) = math.cos(radians)

    matrix
  }

  def rotationY(radians: Double): Matrix = {
    val matrix = IdentityMatrix()

    matrix(0, 0) = math.cos(radians)
    matrix(0, 2) = math.sin(radians)
    matrix(2, 0) = -math.sin(radians)
    matrix(2, 2) = math.cos(radians)

    matrix
  }

  def rotationZ(radians: Double): Matrix = {
    val matrix = IdentityMatrix()

    matrix(0, 0) = math.cos(radians)
    matrix(0, 1) = -math.sin(radians)
    matrix(1, 0) = math.sin(radians)
    matrix(1, 1) = math.cos(radians)

    matrix
  }

  def shearing(xy: Int, xz: Int, yx: Int, yz: Int, zx: Int, zy: Int):Matrix = {
    val matrix = IdentityMatrix()

    matrix(0, 1) = xy
    matrix(0, 2) = xz
    matrix(1, 0) = yx
    matrix(1, 2) = yz
    matrix(2, 0) = zx
    matrix(2, 1) = zy

    matrix
  }
}
