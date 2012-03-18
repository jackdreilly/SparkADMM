package admm

import utils.OptTypes._
import scalala.library.Library._
import scalala.library.Plotting._
import scalala.tensor.dense.DenseMatrix
import utils.{NoisyData, OptFunctions}
import spark.SparkContext

/**
 * Created by IntelliJ IDEA.
 * User: Jojo
 * Date: 18/03/12
 * Time: 00:53
 * To change this template use File | Settings | File Templates.
 */

object LassoExample extends GeneralADMM{

  var lambda: Double = .1


  val xUpdate: UpdateFn = (A, _, b,rho, _, z, u) => (A.t * A + rho) \ (A.t * b + rho :* (z - u))
  val zUpdate: UpdateFn = (_, _, _,rho, x, _, uOld) => OptFunctions.softThreshold(lambda/rho)(x + u)
  val uUpdate: UpdateFn = (_, _, _,_, x, z, uOld) => uOld + x - z

  def solve(A: Matrix,
            b: Vec,
            rho: Double,
            epsR: Double = .001,
            epsS: Double = .001,
            maxIters: Int = 100): Vec = {
    val tvals = linspace(-1/rho*2,1/rho*2,20)
    plot(tvals,OptFunctions.softThreshold(1/rho)(tvals))
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
      val estDiff: Vec = state - estimate
      estDiff.norm(2)
    }.toArray()
    errors.foreach(println)
  }


}
