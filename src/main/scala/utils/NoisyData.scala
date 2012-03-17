package utils

/**
 * Created by IntelliJ IDEA.
 * User: jdr
 * Date: 3/15/12
 * Time: 7:21 PM
 * To change this template use File | Settings | File Templates.
 */


import scalala.tensor.dense._
import scalala.operators.Implicits._
import OptTypes.{Matrix,Vec}


object NoisyData {
  def genData(nSamples: Int, nFeatures: Int): Matrix = {
    DenseMatrix.randn(nSamples, nFeatures)
  }
  def genState(nFeatures: Int): Vec = {
     DenseVectorCol.randn(nFeatures)
  }
  def genOutput(state: Vec, data: Matrix): Vec = {
    val nFeatures = state.size
    val nSamples = data.numRows
    val noisyState = state :* (DenseVectorCol.ones[Double](nFeatures) :- (.01:*DenseVectorCol.rand (nFeatures)))
    val noisyData =  data :* (DenseMatrix.ones[Double](nSamples, nFeatures) :- (.01:*DenseMatrix.rand (nSamples, nFeatures)))
    return noisyData*noisyState
  }

  def main(args: Array[String]) {
    val nF = 3
    val nS = 3
    val data = genData(nS,nF)
    val state = genState(nF)
    val output = data*state
    val noisyOutput = genOutput(state,data)
    println(output)
    println(noisyOutput)
  }
}
