package admmutils

import cern.colt.function.tdouble.DoubleFunction
import cern.jet.math.tdouble.DoubleFunctions
import util.Random
import cern.colt.matrix.tdouble.impl.{SparseDoubleMatrix2D, SparseDoubleMatrix1D, DenseDoubleMatrix1D}
import cern.colt.matrix.tdouble.{DoubleMatrix2D, DoubleFactory2D, DoubleFactory1D, DoubleMatrix1D}
import data.RCV1Data._
import collection.immutable.List
import data.{SlicedDataSet, RCV1Data, SingleSet, DataSet}

/**
 * User: jdr
 * Date: 4/9/12
 * Time: 4:29 PM
 */

object ADMMFunctions {
  def shrinkage(param: Double): DoubleFunction = {
    new DoubleFunction {
      def apply(p1: Double): Double = math.max(0,1-param/math.abs(p1))*p1
    }
  }
  def mean(vecs: Iterable[DoubleMatrix1D]): DoubleMatrix1D = {
    val n = vecs.head.size().toInt
    val out = DoubleFactory1D.dense.make(n,0.0)
    vecs.foreach{case vec => out.assign(vec,DoubleFunctions.plus)}
    out.assign(DoubleFunctions.div(vecs.size.toDouble))
    out
  }
  def sprandnvec(n: Int, sparsity: Double): DoubleMatrix1D = {
    def sample: Boolean = Random.nextDouble() < sparsity
    val out = DoubleFactory1D.sparse.make(n)
    val indices = (1 to n).filter{_ => sample}
    indices.foreach{out.setQuick(_,Random.nextGaussian())}
    out
  }
  def sprandnMatrix(m: Int, n: Int, sparsity: Double): DoubleMatrix2D = {
    def sample: Boolean = Random.nextDouble() < sparsity
    val out = DoubleFactory2D.sparse.make(m,n)
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (sample) out.setQuick(i,j,Random.nextGaussian())
      }
    }
    out
  }


  def main(args: Array[String]) {
    val x = DoubleFactory1D.dense.random(10)
    val y = DoubleFactory1D.dense.random(10)
    val z= mean(List(x,y))
    println(x)
    println(y)
    println(z)
    val w = new DenseDoubleMatrix1D(5)
    w.setQuick(0,1.0)
    w.setQuick(1,2.0)
    w.setQuick(2,-1.0)
    w.setQuick(3,-2.0)
    w.setQuick(4,0.5)
    println(w)
    w.assign(shrinkage(1.5))
    println(w)
  }
}
