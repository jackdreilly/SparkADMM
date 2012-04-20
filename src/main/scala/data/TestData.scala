package data

import cern.jet.math.tdouble.DoubleFunctions
import admmutils.ADMMFunctions
import util.Random
import cern.colt.matrix.tdouble.{DoubleFactory1D, DoubleMatrix1D, DoubleMatrix2D, DoubleFactory2D}
import admm.SLRDistributed
import data.SingleSet

/**
 * User: jdr
 * Date: 4/10/12
 * Time: 2:31 PM
 */

object TestData {
  case class DataSetWithTruth[A,B,T<:DataSet[A,B]](truth: B, noise: T)
  def slrData(m: Int, n: Int, sparsity: Double): DataSetWithTruth[DoubleMatrix2D, DoubleMatrix1D, SingleSet[DoubleMatrix2D,DoubleMatrix1D]] =  {
    val w = ADMMFunctions.sprandnvec(n,sparsity)
    val v = Random.nextGaussian()
    val trueX = DoubleFactory1D.sparse.append(DoubleFactory1D.sparse.make(1,v),w)
    val X = ADMMFunctions.sprandnMatrix(m,n, sparsity)
    val bTrue = X.zMult(w,null)
    bTrue.assign(DoubleFunctions.plus(v))
      .assign(DoubleFunctions.sign)
    //Calculation of A
    val bNoise = bTrue.copy()
    val noise = ADMMFunctions.sprandnvec(m,1.0)
    noise.assign(DoubleFunctions.mult(.01))
    bNoise.assign(noise,DoubleFunctions.plus)
    val B = DoubleFactory2D.sparse.diagonal(bNoise)
    val A = DoubleFactory2D.sparse.make(m,n)
    B.zMult(X,A)
    val out: DataSetWithTruth[DoubleMatrix2D, DoubleMatrix1D,SingleSet[DoubleMatrix2D,DoubleMatrix1D]] = DataSetWithTruth(trueX,SingleSet(A,bNoise))
    out
  }
  def main(args: Array[String]) {


  }
}
