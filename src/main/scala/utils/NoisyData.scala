package utils

/**
 * User: jdr
 * Date: 3/15/12
 * Time: 7:21 PM
 */


import scalala.tensor.dense._
import scalala.operators.Implicits._
import OptTypes.{Mat, Vec}
import scala.util.Random.nextDouble


object NoisyData {
  val scale = 100.0

  def genData(nSamples: Int, nFeatures: Int): Mat = {
    scale :* DenseMatrix.randn(nSamples, nFeatures)
  }

  def genState(nFeatures: Int): Vec = {
    scale :* DenseVectorCol.randn(nFeatures)
  }

  def genSparseState(nFeatures: Int, prob: Double): Vec = genState(nFeatures)
    .map {
    case (par) => zeroer(par, prob)
  }

  def zeroer(param: Double, prob: Double): Double = {
    assert(prob >= 0.0 && prob <= 1.0)
    val zeroit = prob >= nextDouble()
    if (zeroit) param else 0.0
  }

  def genOutput(state: Vec, data: Mat): Vec = {
    val nFeatures = state.size
    val nSamples = data.numRows
    val noisyState = state :* (DenseVectorCol.ones[Double](nFeatures) :- (.01 :* DenseVectorCol.rand(nFeatures)))
    val noisyData = data :* (DenseMatrix.ones[Double](nSamples, nFeatures) :- (.01 :* DenseMatrix.rand(nSamples, nFeatures)))
    noisyData * noisyState
  }

  def main(args: Array[String]) {
    val nF = 3
    val nS = 3
    val data = genData(nS, nF)
    val state = genState(nF)
    val output = data * state
    val noisyOutput = genOutput(state, data)
    println(output)
    println(noisyOutput)
  }
}
