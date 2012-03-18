package utils


import OptTypes.{Vec, Mat}
import scala.math.max

import scalala.tensor.::;


import scalala.library.Library._;


import scalala.library.Plotting._;


/**
 * User: jdr
 * Date: 3/15/12
 * Time: 9:19 PM
 */

object OptFunctions {
  def softThreshold(kappa: Double): (Vec) => Vec = {
    def softKappa(vec: Vec): Vec = {
      vec.map {
        (param: Double) => max(1 - kappa / param.abs, 0) * param
      }
    }
    softKappa
  }

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

  def duplicate[A](single: A, n: Int): Seq[A] = for (i <- 0 until n) yield single


  def main(args: Array[String]) {
    val kappa = 3.0
    val kappaThreshold = softThreshold(kappa)
    val tVals = linspace(-5, 5, 100)
    plot(tVals, kappaThreshold(tVals))

  }

}
