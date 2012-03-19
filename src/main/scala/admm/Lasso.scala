package admm

import utils.NoisyData
import spark.SparkContext
import utils.OptFunctions.{sliceMatrix, sliceVector, softThresholdVec, normalizeMat, normalizeVec}
import utils.OptTypes._
import collection.mutable.MutableList
import scalala.tensor.dense._;
import scalala.operators.Implicits._;

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
    val nIters = 100
    val rho = 1.0
    val lambda = 5.0
    val A = DenseMatrix.randn(nSamples,nFeatures)
    val state = NoisyData.sparsify(DenseVectorCol.randn(nFeatures),sparsity)
    val b: DenseVectorCol[Double] = A*state
    val Atb: DenseVectorCol[Double] = A.t*b
    val AtArhoI =  (A.t*A :+ rho:*DenseMatrix.eye[Double](nFeatures)).toDense
    var x: DenseVectorCol[Double] = DenseVectorCol.zeros[Double](nFeatures)
    var z: DenseVectorCol[Double] = DenseVectorCol.zeros[Double](nFeatures)
    var u: DenseVectorCol[Double] = DenseVectorCol.zeros[Double](nFeatures)
    for (i <- 1 to nIters) {
      x = AtArhoI \ (Atb :+ (rho:*(z :- u)).asInstanceOf[DenseVectorCol[Double]])
      z = softThresholdVec(lambda/rho)(x + u)
      println(z.t)
      u = u + x - z
    }
    println(DenseMatrix(state.data,x.data,z.data))
  }
}

object SparkLasso {
  val zeros = DenseVectorCol.zeros[Double](_)
  val lambda = .0005
  val rho = 1.0
  val maxIters = 1000
  val nMaps = 5

  def solve(A: Mat,  b: Vec) : Vec = {
    // dimension stuff
    val nSamples = A.numRows
    val nFeatures = A.numCols

    // threshold function w/ parameter
    val threshold = softThresholdVec(lambda / rho)

    // launch the context
    val spark = new SparkContext("local", "Lasso Local")

    // initialize the variables that are iterated
    var xs = for (i <- (0 until nMaps)) yield zeros(nFeatures)
    var z = zeros(nFeatures)
    var us = for (i <- (0 until nMaps)) yield zeros(nFeatures)

    // slice up the data
    val dataSlices = sliceMatrix(A, nMaps)
    val outputSlices = sliceVector(b, nMaps)

    // cache the important chunks of data across the maps
    val xAtAFn: Mat => Mat = Ai => (Ai.t * Ai + rho :* DenseMatrix.eye[Double](Ai.numCols)).asInstanceOf[Mat]
    val xAtA = spark.parallelize(dataSlices).map(xAtAFn)
    val xAtb = spark.parallelize(dataSlices.zip(outputSlices)).map(pair => (pair._1.t * pair._2).asInstanceOf[Vec])
    val xDataCache = spark.parallelize(((0 until nMaps), xAtA.toArray(), xAtb.toArray()).zipped.toList).cache()

    // xUpdate: ridge regression
    def xUpdate(ind: Int, t1: Mat, t2: Vec): Vec = {
      t1 \ (t2 + (rho :* (z - us(ind))).asInstanceOf[DenseVectorCol[Double]])
    }

    // the main iteration loops, with x update, z update and u update
    for (_ <- 0 until maxIters) {
      xs = xDataCache.map {
        case (ind, t1, t2) => xUpdate(ind, t1, t2)
      }.toArray.toIndexedSeq
      z = threshold((xs.reduce(_ + _) + us.reduce(_ + _)) :/ nMaps)
      us = spark.parallelize((us, xs).zipped.toList).map {
        case (x, u) => u + x - z
      }.toArray.toIndexedSeq
    }
    // return value
    z
  }
  def main(args: Array[String]) {
    val nSamples = 10000
    val nFeatures = 50
    val sparseness = .5
    val A = normalizeMat(NoisyData.genData(nSamples, nFeatures))
    val trueFeatures = normalizeVec(NoisyData.genSparseState(nFeatures, sparseness))
    val noisyOutput = NoisyData.genOutput(trueFeatures, A)
    val estFeatures = solve(A,noisyOutput)
    println(trueFeatures.t)
    println(estFeatures.t)
  }
}
