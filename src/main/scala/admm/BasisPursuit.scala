package admm


import utils.OptFunctions
import utils.NoisyData


/**
 * User: jdr
 * Date: 3/15/12
 * Time: 9:15 PM
 */

import utils.OptTypes.{Mat, Vec, UpdateFn}
import scalala.tensor.dense.DenseMatrix
import scalala.operators.Implicits._
import scalala.library.Library.linspace


object BasisPursuit extends GeneralADMM {


  val xUpdate: UpdateFn = (A, _, b, _, _, z, u) => {
    val bigA: Mat = A.t * ((A * A.t) \ DenseMatrix.eye[Double](A.numRows))
    val precEye = DenseMatrix.eye[Double](A.numCols)
    val biggerA = bigA * A
    val zDiffu = z - u
    val bigAb = bigA * b
    (precEye - biggerA) * zDiffu + bigAb
  }
  val zUpdate: UpdateFn = (_, _, _, rho, x, _, u) => OptFunctions.softThresholdVec(1 / rho)(x + u)
  val uUpdate: UpdateFn = (_, _, _, _, x, z, uOld) => uOld + x - z

  def solve(A: Mat,
            b: Vec,
            rho: Double = 100.0,
            epsR: Double = .2,
            epsS: Double = .2,
            maxIters: Int = 100): Vec = {
    super.solve(A, -1 :* DenseMatrix.eye[Double](A.numRows, A.numCols), b, rho, epsR, epsS, maxIters)
  }


  def main(args: Array[String]) {
    val data = NoisyData.genData(5, 20)
    val state = NoisyData.genState(20)
    val output = NoisyData.genOutput(state, data)
    val A = data
    val b = output
    for (rho <- linspace(1, 1000, 10)) {
      println("RHO VALUE")
      println(rho)
      println("::::::::::")
      println(BasisPursuit.solve(A, b, rho))
    }
  }


}
