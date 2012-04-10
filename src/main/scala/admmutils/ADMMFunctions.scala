package admmutils

import cern.colt.function.tdouble.DoubleFunction
import cern.colt.matrix.tdouble.{DoubleFactory1D, DoubleMatrix1D}
import cern.jet.math.tdouble.DoubleFunctions
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix1D

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
