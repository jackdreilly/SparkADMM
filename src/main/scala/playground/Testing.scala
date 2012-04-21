package playground

/**
 * User: jdr
 * Date: 3/27/12
 * Time: 4:43 PM
 */

import cern.colt.matrix.tdouble.{DoubleMatrix1D, DoubleFactory1D, DoubleFactory2D}
import cern.jet.math.tdouble.DoubleFunctions
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import scala.util.control.Breaks._
import spark.{SparkContext, HadoopRDD}
import org.apache.hadoop.io.{LongWritable, Text}
import org.apache.hadoop.mapred.{FileInputFormat, InputFormat, TextInputFormat, JobConf}
import org.apache.hadoop.fs.Path
import data.ReutersData.{ReutersRecord, ReutersSet}


object Testing extends App {
  def test1 = {
  val m = 2
  val C = DoubleFactory2D.dense.make(m,m,3.0)
  val x = DoubleFactory1D.dense.make(m,1.0)
  val u = DoubleFactory1D.dense.make(m,3.0)
  val z = DoubleFactory1D.dense.make(m,5.0)
  val rho = .25
  val alpha = .1
  val beta = .5
  val algebra =  new DenseDoubleAlgebra()
  def gradient(x:DoubleMatrix1D): DoubleMatrix1D = {
    val expTerm = C.zMult(x,null)
    expTerm.assign(DoubleFunctions.exp)
    val firstTerm = expTerm.copy()
    firstTerm.assign(DoubleFunctions.plus(1.0))
      .assign(DoubleFunctions.inv)
      .assign(expTerm,DoubleFunctions.mult)
    val secondTerm = x.copy()
    secondTerm.assign(z,DoubleFunctions.minus)

      .assign(u,DoubleFunctions.plus)
      .assign(DoubleFunctions.mult(rho))
    val returnValue = C.zMult(firstTerm,null,1.0,1.0,true)
    returnValue.assign(secondTerm,DoubleFunctions.plus)
    returnValue
  }
  def loss(x: DoubleMatrix1D): Double = {
    val expTerm = C.zMult(x,null)
    expTerm.assign(DoubleFunctions.exp)
      .assign(DoubleFunctions.plus(1.0))
      .assign(DoubleFunctions.log)
    val normTerm = x.copy()
    normTerm.assign(z,DoubleFunctions.minus)
      .assign(u,DoubleFunctions.plus)
    expTerm.zSum() + math.pow(algebra.norm2(normTerm),2)*rho/2
  }
  for (_ <- 1 to 10) {
    val grad = gradient(x)
    val dx = grad.copy()
    dx.assign(DoubleFunctions.neg)
    var t = 1.0
    breakable {
      while (true) {
        val newX = x.copy()
        newX.assign(dx,DoubleFunctions.plusMultSecond(t))
        val lhs = loss(newX)
        val rhs = loss(x) + alpha*t*(dx.zDotProduct(grad))
        if (lhs < rhs) break()
        t*=beta
      }
    }
    x.assign(dx,DoubleFunctions.plusMultSecond(t))
    println(loss(x))
  }
  }

}