package admm

import utils.NoisyData
import spark.SparkContext
import utils.OptFunctions.{sliceMatrix, sliceVector, softThresholdVec}
import scalala.tensor.dense._;
import utils.OptTypes._
import collection.mutable.MutableList

/**
 * User: jdr
 * Date: 3/17/12
 * Time: 4:02 PM
 */


object LocalLasso {
  def main(args: Array[String]) {
    val nSamples = 100
    val nFeatures = 10
    val sparsity = .5
    val A = NoisyData.genData(nSamples, nFeatures)
    val state = NoisyData.genSparseState(nFeatures, sparsity)
    val b = NoisyData.genOutput(state, A)
    var xPrev = NoisyData.genState(nFeatures)
    var xNext = NoisyData.genState(nFeatures)
    var zPrev = NoisyData.genState(nFeatures)
    var zNext = NoisyData.genState(nFeatures)
    var uPrev = NoisyData.genState(nFeatures)
    var uNext = NoisyData.genState(nFeatures)


  }
}

object SparkLasso {
  def main(args: Array[String]) {
    val nSamples = 100
    val nFeatures = 5
    val nMaps = 2
    val nIters = 100
    val rho = 1.0
    val lambda = 1.0
    val sparseness = .5

    val threshold = softThresholdVec(lambda / rho)

    val zValues = new MutableList[Vec]()

    val spark = new SparkContext("local", "Lasso Local")

    val A = NoisyData.genData(nSamples, nFeatures)
    val trueFeatures = NoisyData.genSparseState(nFeatures, sparseness)
    val noisyOutput = NoisyData.genOutput(trueFeatures, A)

    var xs = for (i <- (0 until nMaps)) yield NoisyData.genState(nFeatures)
    var z = NoisyData.genState(nFeatures)
    var us: IndexedSeq[Vec] = for (i <- (0 until nMaps)) yield NoisyData.genState(nFeatures)
    val dataSlices = sliceMatrix(A, nMaps)
    val outputSlices = sliceVector(noisyOutput, nMaps)
    val xAtAFn: Mat => Mat = Ai => (Ai.t * Ai + rho :* DenseMatrix.eye[Double](Ai.numCols)).asInstanceOf[Mat]
    val xAtA = spark.parallelize(dataSlices).map(xAtAFn)
    val xAtb = spark.parallelize(dataSlices.zip(outputSlices)).map(pair => (pair._1.t * pair._2).asInstanceOf[Vec])
    val xDataCache = spark.parallelize(((0 until nMaps), xAtA.toArray(), xAtb.toArray()).zipped.toList).cache()
    def xUpdate(ind: Int, t1: Mat, t2: Vec): Vec = {
      t1 \ (t2 + rho :* (z - us(ind)))
    }
    for (_ <- 0 until nIters) {
      xs = xDataCache.map {
        case (ind, t1, t2) => xUpdate(ind, t1, t2)
      }.toArray.toIndexedSeq
      z = threshold((xs.reduce(_ + _) + us.reduce(_ + _)) :/ nMaps)
      zValues += z
      us = spark.parallelize((us, xs).zipped.toList).map {
        case (x, u) => u + x - z
      }.toArray.toIndexedSeq
    }
    zValues.foreach {
      (zVal: Vec) => println((zVal - trueFeatures).norm(2))
    }
  }
}
