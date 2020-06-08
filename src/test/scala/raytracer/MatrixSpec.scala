package raytracer

import org.scalatest._
import raytracer.DoubleEnhancements._

class MatrixSpec extends FlatSpec with Matchers {
  "Matrix" should "construct and inspect a 4x4 matrix" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(1, 2, 3, 4, 5)
    matrix(1) = Array(5.5, 6.5, 7.5, 8.5)
    matrix(2) = Array(9, 10, 11, 12)
    matrix(3) = Array(13.5, 14.5, 15.5, 16.5)

    (matrix(0)(0) ~= 1) should equal(true)
    (matrix(0)(3) ~= 4) should equal(true)
    (matrix(1)(0) ~= 5.5) should equal(true)
    (matrix(1)(2) ~= 7.5) should equal(true)
    (matrix(2)(2) ~= 11) should equal(true)
    (matrix(3)(0) ~= 13.5) should equal(true)
    (matrix(3)(2) ~= 15.5) should equal(true)
  }

  it should "be able to represent a 2x2 matrix" in {
    val matrix = Matrix(2, 2)
    matrix(0) = Array(-3, 5)
    matrix(1) = Array(1, -2)

    (matrix(0)(0) ~= -3) should equal(true)
    (matrix(0)(1) ~= 5) should equal(true)
    (matrix(1)(0) ~= 1) should equal(true)
    (matrix(1)(1) ~= -2) should equal(true)
  }

  it should "be able to represent a 3x3 matrix" in {
    val matrix = Matrix(3, 3)
    matrix(0) = Array(-3, 5, 0)
    matrix(1) = Array(1, -2, -7)
    matrix(2) = Array(0, 1, 1)

    matrix(0)(0) should equal(-3.0 +- EPSILON)
    matrix(1)(1) should equal(-2.0 +- EPSILON)
    matrix(2)(2) should equal(1.0 +- EPSILON)
  }

  it should "compare matrices" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(1, 2, 3, 4)
    matrix(1) = Array(5, 6, 7, 8)
    matrix(2) = Array(9, 8, 7, 6)
    matrix(3) = Array(5, 4, 3, 2)

    val matrix1 = Matrix(4, 4)
    matrix1(0) = Array(1, 2, 3, 4)
    matrix1(1) = Array(5, 6, 7, 8)
    matrix1(2) = Array(9, 8, 7, 6)
    matrix1(3) = Array(5, 4, 3, 2)

    (matrix == matrix1) should equal(true)

    val matrix2 = Matrix(4, 4)
    matrix2(0) = Array(2, 3, 4, 5)
    matrix2(1) = Array(6, 7, 8, 9)
    matrix2(2) = Array(8, 7, 6, 5)
    matrix2(3) = Array(4, 3, 2, 1)

    (matrix == matrix2) should not equal (true)
  }

  it should "multiply two matrices" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(1, 2, 3, 4)
    matrix(1) = Array(5, 6, 7, 8)
    matrix(2) = Array(9, 8, 7, 6)
    matrix(3) = Array(5, 4, 3, 2)

    val matrix1 = Matrix(4, 4)
    matrix1(0) = Array(-2, 1, 2, 3)
    matrix1(1) = Array(3, 2, 1, -1)
    matrix1(2) = Array(4, 3, 6, 5)
    matrix1(3) = Array(1, 2, 7, 8)

    val expectedResult = Matrix(4, 4)
    expectedResult(0) = Array(20, 22, 50, 48)
    expectedResult(1) = Array(44, 54, 114, 108)
    expectedResult(2) = Array(40, 58, 110, 102)
    expectedResult(3) = Array(16, 26, 46, 42)

    val multiplied = matrix * matrix1

    (multiplied == expectedResult) should equal(true)
  }

  it should "multiple a matrix by a tuple" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(1, 2, 3, 4)
    matrix(1) = Array(2, 4, 4, 2)
    matrix(2) = Array(8, 6, 4, 1)
    matrix(3) = Array(0, 0, 0, 1)

    val tuple = Tuple(1, 2, 3, 1)

    val multiplied = matrix * tuple

    (multiplied == Tuple(18, 24, 33, 1)) should equal(true)
  }

  it should "multiply a matrix by the identity matrix" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(0, 1, 2, 4)
    matrix(1) = Array(1, 2, 4, 8)
    matrix(2) = Array(2, 4, 8, 16)
    matrix(3) = Array(4, 8, 16, 32)

    import Matrix._

    ((matrix * IDENTITY_MATRIX) == matrix) should be(true)
  }

  it should "transpose a matrix" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(0, 9, 3, 0)
    matrix(1) = Array(9, 8, 0, 8)
    matrix(2) = Array(1, 8, 5, 3)
    matrix(3) = Array(0, 0, 5, 8)

    val expectedResult = Matrix(4, 4)
    expectedResult(0) = Array(0, 9, 1, 0)
    expectedResult(1) = Array(9, 8, 8, 0)
    expectedResult(2) = Array(3, 0, 5, 5)
    expectedResult(3) = Array(0, 8, 3, 8)

    (matrix.transpose() == expectedResult) should be(true)
  }

  it should "transposing the identity matrix" in {
    import Matrix.IDENTITY_MATRIX

    (IDENTITY_MATRIX.transpose() == IDENTITY_MATRIX) should be(true)
  }

  it should "calculate the determinant of a 2x2 matrix" in {
    val matrix = Matrix(2, 2)
    matrix(0) = Array(1, 5)
    matrix(1) = Array(-3, 2)

    matrix.determinant should equal(17.0 +- EPSILON)
  }

  it should "submatrix of a 3x3 matrix is a 2x2 matrix" in {
    val matrix = Matrix(3, 3)
    matrix(0) = Array(1, 5, 0)
    matrix(1) = Array(-3, 2, 7)
    matrix(2) = Array(0, 6, -3)

    val expectedResult = Matrix(2, 2)
    expectedResult(0) = Array(-3, 2)
    expectedResult(1) = Array(0, 6)

    (matrix.submatrix(0, 2) == expectedResult) should be(true)
  }

  it should "submatrix of a 4x4 matrix is a 3x3 matrix" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(-6, 1, 1, 6)
    matrix(1) = Array(-8, 5, 8, 6)
    matrix(2) = Array(-1, 0, 8, 2)
    matrix(3) = Array(-7, 1, -1, 1)

    val expectedResult = Matrix(3, 3)
    expectedResult(0) = Array(-6, 1, 6)
    expectedResult(1) = Array(-8, 8, 6)
    expectedResult(2) = Array(-7, -1, 1)

    (matrix.submatrix(2, 1) == expectedResult) should equal(true)
  }

  it should "calculate the minor of a 3x3 matrix" in {
    val matrix = Matrix(3, 3)
    matrix(0) = Array(3, 5, 0)
    matrix(1) = Array(2, -1, -7)
    matrix(2) = Array(6, -1, 5)

    val submatrix = matrix.submatrix(1, 0)

    submatrix.determinant() should equal(25.0 +- EPSILON)
    matrix.minor(1, 0) should equal(25.0 +- EPSILON)
  }

  it should "calculate a cofactor of a 3x3 matrix" in {
    val matrix = Matrix(3, 3)
    matrix(0) = Array(3, 5, 0)
    matrix(1) = Array(2, -1, -7)
    matrix(2) = Array(6, -1, 5)

    matrix.minor(0, 0) should equal(-12.0 +- EPSILON)
    matrix.cofactor(0, 0) should equal(-12.0 +- EPSILON)
    matrix.minor(1, 0) should equal(25.0 +- EPSILON)
    matrix.cofactor(1, 0) should equal(-25.0 +- EPSILON)
  }

  it should "calculate the determinant of a 3x3 matrix" in {
    val matrix = Matrix(3, 3)
    matrix(0) = Array(1, 2, 6)
    matrix(1) = Array(-5, 8, -4)
    matrix(2) = Array(2, 6, 4)

    matrix.cofactor(0, 0) should equal(56.0 +- EPSILON)
    matrix.cofactor(0, 1) should equal(12.0 +- EPSILON)
    matrix.cofactor(0, 2) should equal(-46.0 +- EPSILON)
    matrix.determinant() should equal(-196.0 +- EPSILON)
  }

  it should "calculate the determinant of a 4x4 matrix" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(-2, -8, 3, 5)
    matrix(1) = Array(-3, 1, 7, 3)
    matrix(2) = Array(1, 2, -9, 6)
    matrix(3) = Array(-6, 7, 7, -9)

    matrix.cofactor(0, 0) should equal(690.0 +- EPSILON)
    matrix.cofactor(0, 1) should equal(447.0 +- EPSILON)
    matrix.cofactor(0, 2) should equal(210.0 +- EPSILON)
    matrix.cofactor(0, 3) should equal(51.0 +- EPSILON)
    matrix.determinant() should equal(-4071.0 +- EPSILON)
  }

  it should "test an invertible matrix for invertibility" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(6, 4, 4, 4)
    matrix(1) = Array(5, 5, 7, 6)
    matrix(2) = Array(4, -9, 3, -7)
    matrix(3) = Array(9, 1, 7, -6)

    matrix.determinant() should equal(-2120.0 +- EPSILON)
    matrix.invertible() should be(true)
  }

  it should "test a non-invertible matrix for invertibility" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(-4, 2, -2, -3)
    matrix(1) = Array(9, 6, 2, 6)
    matrix(2) = Array(0, -5, 1, -5)
    matrix(3) = Array(0, 0, 0, 0)

    matrix.determinant() should equal(0.0 +- EPSILON)
    matrix.invertible() should be(false)
  }

  it should "calculate the inverse of a matrix" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(-5, 2, 6, -8)
    matrix(1) = Array(1, -5, 1, 8)
    matrix(2) = Array(7, 7, -6, -7)
    matrix(3) = Array(1, -3, 7, 4)

    val inverse = matrix.inverse()

    val expectedInverse = Matrix(4, 4)
    expectedInverse(0) = Array(0.21805, 0.45113, 0.24060, -0.04511)
    expectedInverse(1) = Array(-0.80827, -1.45677, -0.44361, 0.52068)
    expectedInverse(2) = Array(-0.07895, -0.22368, -0.05263, 0.19737)
    expectedInverse(3) = Array(-0.52256, -0.81391, -0.30075, 0.30639)

    matrix.determinant() should equal(532.0 +- EPSILON)
    matrix.cofactor(2, 3) should equal(-160.0 +- EPSILON)
    inverse(3)(2) should equal(-160.0 / 532.0 +- EPSILON)
    matrix.cofactor(3, 2) should equal(105.0 +- EPSILON)
    inverse(2)(3) should equal(105.0 / 532.0 +- EPSILON)

    (inverse == expectedInverse) should be(true)
  }

  it should "calculate the inverse of another matrix" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(8, -5, 9, 2)
    matrix(1) = Array(7, 5, 6, 1)
    matrix(2) = Array(-6, 0, 9, 6)
    matrix(3) = Array(-3, 0, -9, -4)

    val expectedInverse = Matrix(4, 4)
    expectedInverse(0) = Array(-0.15385, -0.15385, -0.28205, -0.53846)
    expectedInverse(1) = Array(-0.07692, 0.12308, 0.02564, 0.03077)
    expectedInverse(2) = Array(0.35897, 0.35897, 0.43590, 0.92308)
    expectedInverse(3) = Array(-0.69231, -0.69231, -0.76923, -1.92308)

    (matrix.inverse() == expectedInverse) should be(true)
  }

  it should "calculate the inverse of a third matrix" in {
    val matrix = Matrix(4, 4)
    matrix(0) = Array(9, 3, 0, 9)
    matrix(1) = Array(-5, -2, -6, -3)
    matrix(2) = Array(-4, 9, 6, 4)
    matrix(3) = Array(-7, 6, 6, 2)

    val expectedInverse = Matrix(4, 4)
    expectedInverse(0) = Array(-0.04074, -0.07778, 0.14444, -0.22222)
    expectedInverse(1) = Array(-0.07778, 0.03333, 0.36667, -0.33333)
    expectedInverse(2) = Array(-0.02901, -0.14630, -0.10926, 0.12963)
    expectedInverse(3) = Array(0.17778, 0.06667, -0.26667, 0.33333)

    (matrix.inverse() == expectedInverse) should be(true)
  }

  it should "multiply a product by it's inverse" in {
    val A = Matrix(4, 4)
    A(0) = Array(3, -9, 7, 3)
    A(1) = Array(3, -8, 2, -9)
    A(2) = Array(-4, 4, 4, 1)
    A(3) = Array(-6, 5, -1, 1)

    val B = Matrix(4, 4)
    B(0) = Array(8, 2, 2, 2)
    B(1) = Array(3, -1, 7, 0)
    B(2) = Array(7, 0, 5, 4)
    B(3) = Array(6, -2, 0, 5)

    val C = A * B

    ((C * B.inverse()) == A) should be(true)
  }
}
