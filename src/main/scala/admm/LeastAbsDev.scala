package admm

import scalala.tensor.dense.DenseVectorCol
import utils.OptTypes.{Vec, Matrix}
import utils.OptFunctions
import utils.NoisyData
import spark.SparkContext
import admm.GeneralADMM


/**
 * Created by IntelliJ IDEA.
 * User: jdr
 * Date: 3/15/12
 * Time: 9:15 PM
 * To change this template use File | Settings | File Templates.
 */

import utils.OptTypes.{Matrix, Vec, UpdateFn}
import scalala.tensor.dense.DenseMatrix
import scalala.operators.Implicits._


object LeastAbsDev extends GeneralADMM {



  val xUpdate: UpdateFn = (A, _, b,_, _, z, u) => A \ (b + z - u)
  val zUpdate: UpdateFn = (A, _, b,rho, x, _, u) => OptFunctions.softThreshold(1/rho)(A*x-b+u)
  val uUpdate: UpdateFn = (A, _, b,_, x, z, uOld) => uOld + A*x - z - b
  def solve(A: Matrix,
            b: Vec,
            rho: Double = .5,
            epsR: Double = .2,
            epsS: Double = .2,
            maxIters: Int = 100): Vec = {
    super.solve(A,-1:*DenseMatrix.eye[Double](b.size,b.size),b,rho,epsR,epsS,maxIters)
  }


  def main(args: Array[String]) {
    val data = NoisyData.genData(100,10)
    val state = NoisyData.genState(10)
    val output = NoisyData.genOutput(state,data)
    val A = data
    val b = output
    val spark = new SparkContext("local", "SparkPi")
    val x = LeastAbsDev.solve(A,b)
    println(x)
    val difference: Vec = A*x-b
    println(difference.norm(2))
    println(b.norm(2))
  }


}
