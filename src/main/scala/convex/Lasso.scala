package convex

import scalala.tensor.dense.DenseVectorCol
import utils.OptTypes.{Vec, Matrix}
import scalala.operators.Implicits._
import utils.OptFunctions
import utils.NoisyData
import spark.SparkContext


/**
 * Created by IntelliJ IDEA.
 * User: jdr
 * Date: 3/15/12
 * Time: 9:15 PM
 * To change this template use File | Settings | File Templates.
 */

object Lasso {

  def lassoSolve(A: Matrix,
                 b: Vec,
                 epsR: Double = .4,
                 epsS: Double = .4,
                 rho: Double = .5,
                 maxIters: Int = 100): Vec = {
    val nSamples = A.numRows
    val nFeatures = A.numCols
    var lastX = DenseVectorCol.zeros[Double](nFeatures)
    var lastZ = DenseVectorCol.zeros[Double](nSamples)
    var lastU = DenseVectorCol.zeros[Double](nSamples)
    var newX = DenseVectorCol.zeros[Double](nFeatures)
    var newZ = DenseVectorCol.zeros[Double](nSamples)
    var newU = DenseVectorCol.zeros[Double](nSamples)
    var doTerm = false
    for (iter <- 1 to maxIters) {
      lastX = newX
      newX = xUpdate(A,b,newZ,newU)
      lastZ = newZ
      newZ = zUpdate(A,b,newX,newU,rho)
      lastU = newU
      newU = uUpdate(A,b,newX,newZ,lastU)
      doTerm = terminate(residualPrimal(A,b,newZ,newX),epsR,residualDual(A,newZ,lastZ,rho),epsS)
      if (doTerm)
        return newX
      println(residualPrimal(A,b,newZ,newX).norm(2))

    }
    return newX
  }

  def xUpdate(A: Matrix,
              b: Vec,
              z: Vec,
              u: Vec ): Vec =  {
    val v = b + z - u
    A \ v
  }

  def zUpdate(A: Matrix, b: Vec, x: Vec, u: Vec, rho: Double): Vec = {
    OptFunctions.softThreshold(1/rho)(A*x-b+u)
  }

  def uUpdate(A: Matrix, b: Vec, x: Vec, z: Vec, uOld: Vec): Vec = {
    uOld + A*x - z - b
  }

  def residualPrimal(A: Matrix,
                     b: Vec,
                     z: Vec,
                     x: Vec ): Vec = {
    A*x - z - b
  }
  def residualDual(A: Matrix,
                   zNew: Vec,
                   zOld: Vec,
                   rho: Double): Vec = {
    -rho:*A.t*(zNew - zOld)
  }
  def terminate(r: Vec,
                epsR: Double,
                s: Vec,
                epsS: Double): Boolean =  {
    return (r.norm(2) < epsR && s.norm(2) < epsS)
  }



  def main(args: Array[String]) {
    val data = NoisyData.genData(4000,100)
    val state = NoisyData.genState(100)
    val output = NoisyData.genOutput(state,data)
    val A = data
    val b = output
    val spark = new SparkContext("local", "SparkPi")
    val x = lassoSolve(A,b)
    println(x)
    val difference: Vec = A*x-b
    println(difference.norm(2))
    println(b.norm(2))
  }


}
