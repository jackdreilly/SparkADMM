package admm

/**
 * Created by IntelliJ IDEA.
 * User: Boris
 * Date: 20/03/12
 * Time: 22:16
 * To change this template use File | Settings | File Templates.
 */

import cern.colt.matrix.tdouble.impl.{SparseDoubleMatrix1D, SparseDoubleMatrix2D}
import cern.jet.math.tdouble.DoubleFunctions
import data.RCV1Data
import cern.colt.matrix.tdouble.{DoubleFactory1D, DoubleMatrix1D}

object LocalLogR {
  type DataPair = (SparseDoubleMatrix2D, SparseDoubleMatrix1D)
  val ITERATIONS = 5

  def solve(sparseData: SparseDoubleMatrix2D, sparseOutput: SparseDoubleMatrix1D): DoubleMatrix1D ={
    val nSamples = sparseData.rows()
    val nFeatures = sparseData.columns()
    // Initialize w to a random value
    val w = DoubleFactory1D.sparse.make(nFeatures,0.0)
    for (i <- 1 to ITERATIONS) {
      println("On iteration " + i)
      val gradient = DoubleFactory1D.sparse.make(nFeatures,0.0)
      for (j <- 0 until nSamples) {
        val b = sparseOutput.getQuick(j)
        val bprime = if (b == 0) -1 else 1
        val p = sparseData.viewRow(j)
        val scale = (1 / (1 + math.exp(-bprime * w.zDotProduct(p))) - 1) * bprime
        gradient.assign(p, DoubleFunctions.multSecond(scale))
      }
      w.assign(gradient, DoubleFunctions.minus)
      println("card")
      println(w.cardinality())
    }
    w
  }

  def learnAndCheck(trainSet: DataPair, testSet: DataPair) {
    val wTrain = solve(trainSet._1, trainSet._2)
    val ATest = testSet._1
    val bTest = testSet._2
    val bEst = ATest.zMult(wTrain,null)
    println(bEst.cardinality())
    bEst.assign(DoubleFunctions.sign)
    bTest.assign(DoubleFunctions.mult(2.0))
         .assign(DoubleFunctions.minus(1.0))
         .assign(bEst,DoubleFunctions.minus)
    println(1.0*bTest.cardinality()/bTest.size())
  }

  def main(args: Array[String]) {
    val nSlices = 1
    val nDocs = args(0).toInt
    val nFeatures = args(1).toInt
    val A = RCV1Data.rcv1IDF(nDocs,2,nFeatures)
    val b = RCV1Data.labels(0,nDocs,2)
    val zippers = A.zip(b)
    learnAndCheck(zippers(0),zippers(1))
  }

}
