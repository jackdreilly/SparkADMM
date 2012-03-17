package admm

import utils.OptTypes._
import scalala.tensor.dense.DenseVectorCol
import scalala.operators.Implicits._


/**
 * Created by IntelliJ IDEA.
 * User: jdr
 * Date: 3/16/12
 * Time: 1:11 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class GeneralADMM {

  val xUpdate: UpdateFn
  val zUpdate: UpdateFn
  val uUpdate: UpdateFn

  def solve(A: Matrix,
            B: Matrix,
            c: Vec,
            rho: Double,
            epsR: Double,
            epsS: Double,
            maxIters: Int): Vec = {
    val nSamples = A.numRows
    val nFeatures = A.numCols
    val zFeatures = B.numCols
    var lastX = DenseVectorCol.zeros[Double](nFeatures)
    var lastZ = DenseVectorCol.zeros[Double](zFeatures)
    var lastU = DenseVectorCol.zeros[Double](zFeatures)
    var newX = DenseVectorCol.zeros[Double](nFeatures)
    var newZ = DenseVectorCol.zeros[Double](zFeatures)
    var newU = DenseVectorCol.zeros[Double](zFeatures)
    var doTerm = false
    for (iter <- 1 to maxIters) {
      lastX = newX
      newX = xUpdate(A,B,c,rho,lastX,newZ,newU)
      lastZ = newZ
      newZ = zUpdate(A,B,c,rho,newX,lastZ,newU)
      lastU = newU
      newU = uUpdate(A,B,c,rho,newX,newZ,lastU)
      if (terminate(A,B,c,lastZ,newZ,newX,rho,epsR,epsS)) return newX
//      println(residualPrimal(A,B,c,newZ,newX).norm(2))
    }
    return newX
  }


  def residualPrimal(A: Matrix,
                     B: Matrix,
                     c: Vec,
                     z: Vec,
                     x: Vec ): Vec = {
    A*x + B*z - c
  }
  def residualDual(A: Matrix,
                   B: Matrix,
                   zNew: Vec,
                   zOld: Vec,
                   rho: Double): Vec = {
    rho:*A.t*B*(zNew - zOld)
  }
  def terminate(A: Matrix,
                B: Matrix,
                c: Vec,
                zNew: Vec,
                zOld: Vec,
                x: Vec,
                rho: Double,
                epsR: Double,
                epsS: Double): Boolean =  {
    (residualPrimal(A,B,c,zNew,x).norm(2) < epsR) &&
      (residualDual (A,B,zNew,zOld,rho).norm(2) < epsS)
  }
}
