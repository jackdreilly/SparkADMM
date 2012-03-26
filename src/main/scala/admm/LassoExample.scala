package admm

import utils.OptTypes._
import scalala.library.Library._
import scalala.tensor.dense.{DenseMatrix, DenseVector}
import scalala.operators.Implicits._
import utils.{NoisyData, OptFunctions}

/**
 * User: Jojo
 * Date: 18/03/12
 * Time: 00:53
 */

object LassoExample extends GeneralADMM {

  var lambda: Double = .1


  val xUpdate: UpdateFn = (A, _, b, rho, _, z, u) => {
    val bigA: Mat = A.t * A
    val bigAA: Mat = bigA + rho :* DenseMatrix.eye[Double](A.numCols, A.numCols)
    val bigB: Vec = A.t * b + rho :* (z - u)
    bigAA \ (bigB)
  }

  val zUpdate: UpdateFn = (_, _, _, rho, x, _, u) => OptFunctions.softThresholdVec(lambda / rho)(x + u)
  val uUpdate: UpdateFn = (_, _, _, _, x, z, uOld) => uOld + x - z

  def solve(A: Mat,
            b: Vec,
            rho: Double,
            epsR: Double = .00001,
            epsS: Double = .00001,
            maxIters: Int = 100): Vec = {
    val tvals = linspace(-1 / rho * 2, 1 / rho * 2, 20)
    super.solve(DenseMatrix.eye[Double](A.numCols), -1 :* DenseMatrix.eye[Double](A.numCols), DenseVector.zeros[Double](A.numCols), rho, epsR, epsS, maxIters)
  }

  def main(args: Array[String]) {
    val data = NoisyData.genData(5, 5)
    val state: Vec = NoisyData.genSparseState(5, .5)
    println("state:")
    println(state)
    println()
    val output = NoisyData.genOutput(state, data)
    val A = data
    println(A)
    println()
    val b = output
    println(b)
    val rho = 1.0
    println()
    val estimate = LassoExample.solve(A, b, rho)
    println(estimate)
  }


}
