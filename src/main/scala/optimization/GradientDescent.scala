package optimization

import cern.jet.math.tdouble.DoubleFunctions
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix1D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.{DoubleFactory1D, DoubleMatrix1D}

/**
 * User: jdr
 * Date: 4/6/12
 * Time: 1:57 PM
 */

object GradientDescent {
  val alg = new DenseDoubleAlgebra()
  def solve(fn: DoubleMatrix1D => Double,
            gradFn: DoubleMatrix1D => DoubleMatrix1D,
            init: DoubleMatrix1D,
            maxIter: Int = 1000): DoubleMatrix1D = {
    def backtrackingLineSearch(x: DoubleMatrix1D, dx: DoubleMatrix1D, alpha: Double, beta: Double): Double = {
      val fx = fn(x)
      val grad = gradFn(x)
      val gradDot = dx.zDotProduct(grad)
      def helper(t: Double): Double = {
        def rhs() = gradDot*alpha*t + fx
        def lhs() = {
          val temp = x.copy()
          temp.assign(dx,DoubleFunctions.plusMultSecond(t))
          fn(temp)
        }
        val rh = rhs()
        val lh = lhs()
//        println("left hand: " + lh.toString)
//        println("right hand: " + rh.toString)
        if (lhs() > rhs()) helper(beta*t) else t
      }
      helper(1.0)
    }
    def stoppingCriteria(x: DoubleMatrix1D, tol: Double ): Boolean = {
      val errorEstimate = alg.norm2(gradFn(x))
      println(x.getQuick(0).toString + "," + x.getQuick(1).toString )
      errorEstimate <= tol
    }
    val alpha = .1
    val beta = .7
    val tol = 1e-3
    val x = init.copy()
    for (_ <- 1 to maxIter; if !stoppingCriteria(x,tol)) {
      val dx = gradFn(x)
      dx.assign(DoubleFunctions.neg)
      dx.assign(DoubleFunctions.div(alg.norm2(dx)))
      val t = backtrackingLineSearch(x, dx, alpha, beta)
      x.assign(dx,DoubleFunctions.multSecond(t))
    }
    x
  }

  def mainGamma() {
    val gamma = 3.0
    val fn = (x: DoubleMatrix1D) => {
      .5*(math.pow(x.getQuick(0),2) + gamma*math.pow(x.getQuick(1),2))
    }
    val dfn = (x: DoubleMatrix1D) => {
      val out = x.copy()
      out.setQuick(1,out.getQuick(1)*gamma)
      out
    }
    val x = DoubleFactory1D.dense.make(2, 50.0)
    val y = GradientDescent.solve(fn,dfn, x, 100)
  }
  def mainExp() {
    val fn = (x: DoubleMatrix1D) => {
      val x1 = x.getQuick(0)
      val x2 = x.getQuick(1)
      math.exp(x1 + 3*x2 - .1) +math.exp(x1 - 3*x2 - .1) +math.exp(-x1  - .1)
    }
    val dfn = (x: DoubleMatrix1D) => {
      val out = DoubleFactory1D.dense.make(2)
      val x1 = x.getQuick(0)
      val x2 = x.getQuick(1)
      out.setQuick(0,math.exp(x1 + 3*x2 - .1) +math.exp(x1 - 3*x2 - .1) -math.exp(-x1  - .1))
      out.setQuick(1,3*math.exp(x1 + 3*x2 - .1) -3*math.exp(x1 - 3*x2 - .1))
      out
    }
    val x = DoubleFactory1D.dense.make(2, 0.0)
    val y = GradientDescent.solve(fn,dfn, x, 100)
  }
  def main(args: Array[String]) {
    mainExp()
  }
}
