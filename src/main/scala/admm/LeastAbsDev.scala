package admm

/**
 * Created by IntelliJ IDEA.
 * User: jdr
 * Date: 3/15/12
 * Time: 9:15 PM
 */

import utils.OptFunctions
import utils.NoisyData
import spark.SparkContext
import utils.OptTypes.{Matrix, Vec, UpdateFn}
import scalala.tensor.dense.DenseMatrix
import scalala.operators.Implicits._
import scalala.library.Library.linspace
import scalala.library.Plotting._


object LeastAbsDev extends GeneralADMM {



  val xUpdate: UpdateFn = (A, _, b,_, _, z, u) => A \ (b + z - u)
  val zUpdate: UpdateFn = (A, _, b,rho, x, _, u) => OptFunctions.softThreshold(1/rho)(A*x-b+u)
  val uUpdate: UpdateFn = (A, _, b,_, x, z, uOld) => uOld + A*x - z - b
  def solve(A: Matrix,
            b: Vec,
            rho: Double,
            epsR: Double = .001,
            epsS: Double = .001,
            maxIters: Int = 100): Vec = {
    val tvals = linspace(-1/rho*2,1/rho*2,20)
    title(rho.toString)
    super.solve(A,-1:*DenseMatrix.eye[Double](b.size,b.size),b,rho,epsR,epsS,maxIters)
  }


  def main(args: Array[String]) {
    val data = NoisyData.genData(200,50)
    val state: Vec = NoisyData.genState(50)
    val output = NoisyData.genOutput(state,data)
    val A = data
    val b = output
    val spark = new SparkContext("local", "SparkPi")
    val rhos = linspace(.001,.1,10)
    val errors = spark.parallelize(rhos.toList,5).map { (rho: Double) =>
      val estimate = LeastAbsDev.solve(A,b,rho)
      val estDiff: Vec = state :- estimate
      estDiff.norm(2)
    }.toArray()
    errors.foreach(println)
  }


}
