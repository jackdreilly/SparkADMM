package admm

/**
 * Created by IntelliJ IDEA.
 * User: Boris
 * Date: 20/03/12
 * Time: 22:16
 * To change this template use File | Settings | File Templates.
 */

import cern.colt.matrix.impl.{SparseDoubleMatrix1D, SparseDoubleMatrix2D}

object LocalLogR {
  val ITERATIONS = 5

  def main(args: Array[String]) {
    val nSamples = 10
    val nFeatures = 20
    val sparseData = new SparseDoubleMatrix2D(nSamples,nFeatures)
    val sparseOutput = new SparseDoubleMatrix1D(nSamples)


    var p = new SparseDoubleMatrix1D(nFeatures)

    // Initialize w to a random value
    var w = new SparseDoubleMatrix1D(nFeatures)
    println("Initial w: " + w)

    for (i <- 1 to ITERATIONS) {
      println("On iteration " + i)
      var gradient = new SparseDoubleMatrix1D(nFeatures)
      for (j <- 1 to nSamples) {
        for (k <- 1 to  nFeatures) {
          p.setQuick(k,sparseData.getQuick(j,k))
        }
       val scale = (1 / (1 + math.exp(-sparseOutput.getQuick (j) * w.zDotProduct(p))) - 1) * sparseOutput.getQuick (j)

       gradient.assign(p, cern.jet.math.Functions.plusMult(scale))
      }
      w.assign(gradient, cern.jet.math.Functions.minus)
    }

    println("Final w: " + w)
  }

}
