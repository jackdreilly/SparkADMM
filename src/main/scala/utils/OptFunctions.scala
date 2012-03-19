package utils


import OptTypes.{Vec, Mat}
import scala.math.max

import scalala.tensor.::;


import scalala.library.Library._;


import scalala.library.Plotting._
import scalala.tensor.mutable.Tensor
import scalala.tensor.dense.DenseVectorCol
;


/**
 * User: jdr
 * Date: 3/15/12
 * Time: 9:19 PM
 */

object OptFunctions {
  def softThreshold(kappa: Double): Double => Double = {
    (param: Double) => max(0, param - kappa) - max (0, -param - kappa)
  }

  val softThresholdVec = (kappa: Double) => (vec: Vec) => vec.map(softThreshold(kappa))

  def sliceMatrix(mat: Mat, nSlices: Int): Seq[Mat] = {
    assert(mat.numRows % nSlices == 0)
    val sliceWidth: Int = mat.numRows / nSlices
    for (i <- 0 until nSlices) yield mat(i * sliceWidth until (i + 1) * sliceWidth, ::).toDense.asInstanceOf[Mat]
  }

  def sliceVector(vec: Vec, nSlices: Int): Seq[Vec] = {
    assert(vec.size % nSlices == 0)
    val sliceWidth: Int = vec.size / nSlices
    for (i <- 0 until nSlices) yield vec(i * sliceWidth until (i + 1) * sliceWidth).toDense.asInstanceOf[Vec]
  }
  val normalizeMat = (mat: Mat) => mat:/norm(mat.data,2)
  val normalizeVec = (vec: Vec) => vec:/vec.norm(2)

  def duplicate[A](single: A, n: Int): Seq[A] = for (i <- 0 until n) yield single


  def main(args: Array[String]) {
    val kappa = 3.0
    val kappaThreshold = softThresholdVec(kappa)
    val test = DenseVectorCol[Double](-5.0,2.0,-2.0,5.0,3.1)
    println(test.t)
    println(softThresholdVec(kappa)(test).t)

  }

}
