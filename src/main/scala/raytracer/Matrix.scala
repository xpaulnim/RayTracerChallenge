package raytracer

import raytracer.DoubleEnhancements._

case class Matrix(rows: Int, cols: Int) {
  private val _matrix = Array.ofDim[Double](rows, cols)

  def apply(row: Int)(col: Int): Double = {
    _matrix(row)(col)
  }

  def update(row: Int, col: Int, d: Double): Unit = {
    _matrix(row)(col) = d
  }

  def update(row: Int, array: Array[Double]): Unit = {
    _matrix(row) = array
  }

  def ==(other: Matrix): Boolean = {
    if ((this.rows != other.rows) && (this.cols != other.cols)) return false

    (this._matrix zip other._matrix).forall(a => {
      a._1.length == a._2.length && (a._1 zip a._2).forall(b => b._1 ~= b._2)
    })
  }

  def *(other: Matrix): Matrix = {
    // TODO: Multiply arbitrary sized matrix
    assert((this.rows == other.rows) && (this.cols == other.cols))

    val result = Matrix(this.rows, this.cols)

    for (row <- 0 until this.rows) {
      for (col <- 0 until this.cols) {
        result(row, col) = this (row)(0) * other(0)(col) +
          this (row)(1) * other(1)(col) +
          this (row)(2) * other(2)(col) +
          this (row)(3) * other(3)(col)
      }
    }

    result
  }

  def *(tuple: Tuple): Tuple = {
    assert(rows == 4 && cols == 4)

    val tupleArr = Array(tuple.x, tuple.y, tuple.z, tuple.w)

    val x = (this._matrix(0) zip tupleArr).foldLeft(0.0)((prod, n) => prod + (n._1 * n._2))
    val y = (this._matrix(1) zip tupleArr).foldLeft(0.0)((prod, n) => prod + (n._1 * n._2))
    val z = (this._matrix(2) zip tupleArr).foldLeft(0.0)((prod, n) => prod + (n._1 * n._2))
    val w = (this._matrix(3) zip tupleArr).foldLeft(0.0)((prod, n) => prod + (n._1 * n._2))

    Tuple(x, y, z, w)
  }

  def transpose(): Matrix = {
    val matrix = Matrix(cols, rows)

    _matrix.transpose.zipWithIndex.foreach(a => matrix(a._2) = a._1)

    matrix
  }

  def determinant(): Double = {
    if (this.rows == 2 && this.cols == 2) {
      (_matrix(0)(0) * _matrix(1)(1)) - (_matrix(0)(1) * _matrix(1)(0))
    } else {
      _matrix(0).zipWithIndex.foldLeft(0.0)((det, a) => det + a._1 * cofactor(0, a._2))
    }
  }

  def submatrix(row: Int, col: Int): Matrix = {
    if (row > this.rows || row < 0 || col > this.cols || col < 0) {
      return this
    }

    val matrix = Matrix(this.rows - 1, this.cols - 1)

    _matrix.zipWithIndex
      .filter(_._2 != row)
      .map(_._1.zipWithIndex.filter(_._2 != col).map(_._1))
      .zipWithIndex
      .foreach(a => matrix(a._2) = a._1)

    matrix
  }

  def minor(row: Int, col: Int): Double = {
    submatrix(row, col).determinant()
  }

  def cofactor(row: Int, col: Int): Double = {
    if ((row + col) % 2 == 0) minor(row, col) else -minor(row, col)
  }

  def invertible(): Boolean = {
    determinant() != 0
  }

  def inverse(): Matrix = {
    assert(this.invertible())

    // 1. matrix of cofactors of each of original elements
    // 2. transpose of cofactor matrix
    // 3. divide each element by determinant

    val matrix = Matrix(rows, cols)

    for(row <- 0 until rows) {
      for (col <- 0 until cols) {
        matrix(col, row) = this.cofactor(row, col) / this.determinant()
      }
    }

    matrix
  }
}

object Matrix {
  val IDENTITY_MATRIX: Matrix = {
    val matrix = Matrix(4, 4)

    matrix(0) = Array(1, 0, 0, 0)
    matrix(1) = Array(0, 1, 0, 0)
    matrix(2) = Array(0, 0, 1, 0)
    matrix(3) = Array(0, 0, 0, 1)

    matrix
  }
}
