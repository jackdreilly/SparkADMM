package admm

/**
 * User: jdr
 * Date: 4/4/12
 * Time: 12:33 PM
 */


import cern.colt.matrix.tdouble.impl._
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.jet.math.tdouble.DoubleFunctions
import cern.colt.matrix.tdouble.{DoubleFactory1D, DoubleMatrix2D, DoubleFactory2D, DoubleMatrix1D}
import scala.util.control.Breaks._
import cern.colt.function.tdouble.DoubleFunction
import data.RCV1Data.{labels, rcv1IDF}
import scalala.library.Plotting.plot
import scalala.library.Library.linspace

object SparseLogisticRegression {
  var lambda = .3
  var rho = .01
  val algebra = new DenseDoubleAlgebra()
  val max_iter = 100
  val shrinkage = new DoubleFunction() {
    def apply(p1: Double): Double = {
      math.max(0, 1 - lambda / rho / math.abs(p1)) * p1
    }
    def plotShit() {
      val t = linspace(-3,3,100)
      plot(t,t.map(apply(_)))
    }
  }

  def solve(A: SparseDoubleMatrix2D, b: SparseDoubleMatrix1D): DoubleMatrix1D = {
    val n = A.columns()
    val m = A.rows()
    val x = DoubleFactory1D.dense.make(n + 1, 1.0)
    val u = DoubleFactory1D.dense.make(n + 1, 1.0)
    val z = DoubleFactory1D.dense.make(n + 1, 1.0)
    val negA = A.copy()
    negA.assign(DoubleFunctions.neg)
    val negb = DoubleFactory2D.sparse.make(m, 1)
    negb.viewColumn(0).assign(b).assign(DoubleFunctions.neg)
    val C = DoubleFactory2D.sparse.appendColumns(negb, negA)

    def xUpdateNewton() {
      def gradient(x: DoubleMatrix1D): DoubleMatrix1D = {
        val expTerm = new DenseDoubleMatrix1D(m)
        C.zMult(x, expTerm, 1.0, 1.0, false)
        expTerm.assign(DoubleFunctions.exp)
        val t1 = expTerm.copy()
        t1.assign(DoubleFunctions.plus(1.0)).assign(expTerm, DoubleFunctions.div).assign(DoubleFunctions.inv)
        val t2 = C.zMult(t1, null, 1.0, 1.0, true)
        val t3 = x.copy()
        t3.assign(z, DoubleFunctions.minus).assign(u, DoubleFunctions.plus).assign(DoubleFunctions.mult(rho))
        val grad = t2.copy()
        grad.assign(t3, DoubleFunctions.plus)
        grad
      }
      def hessian(x: DoubleMatrix1D): DoubleMatrix2D = {
        val expTerm = new DenseDoubleMatrix1D(m)
        C.zMult(x, expTerm, 1.0, 1.0, false)
        expTerm.assign(DoubleFunctions.exp)
        val t1 = expTerm.copy()
        t1.assign(DoubleFunctions.plus(1.0)).assign(DoubleFunctions.square).assign(expTerm, DoubleFunctions.div).assign(DoubleFunctions.inv)
        val diag = DoubleFactory2D.dense.diagonal(t1)
        val tmp = C.zMult(diag, null, 1.0, 1.0, true, false)
        val sln = tmp.zMult(C, null, 1.0, 1.0, false, false)
        sln.assign(DoubleFunctions.plus(rho))
        sln
      }
      def loss(x: DoubleMatrix1D): Double = {
        val expTerm = new DenseDoubleMatrix1D(m)
        C.zMult(x, expTerm, 1.0, 1.0, false)
        expTerm.assign(DoubleFunctions.exp)
        val t1 = expTerm.copy()
        t1.assign(DoubleFunctions.plus(1.0)).assign(DoubleFunctions.log)
        val t2 = x.copy()
        t2.assign(z, DoubleFunctions.minus).assign(u, DoubleFunctions.plus)
        val norm = algebra.norm2(t2)
        norm * norm * rho / 2 + t1.zSum()
      }
      val alpha = .1
      val beta = .5
      val tol = 1e-5
      val max_iter = 50
      breakable {
        for (_ <- 1 to max_iter) {
          val fx = loss(x)
          val gx = gradient(x)
          val gmat = DoubleFactory2D.dense.make(n + 1, 1)
          gmat.viewColumn(0).assign(gx)
          val hx = hessian(x)
          val stuff = algebra.rank(hx)
          val dx = algebra.solve(hx, gmat).viewColumn(0)
          val dfx = dx.zDotProduct(gx)
          if (math.abs(dfx) < tol)
            break
          var t = 1.0
          while ( {
            val scaledDx = dx.copy()
            scaledDx.assign(DoubleFunctions.mult(t))
            val diffX = x.copy()
            diffX.assign(scaledDx, DoubleFunctions.plus)
            loss(diffX) > fx + alpha * t * dfx
          }) {
            t *= beta
          }
          val scaledDx = dx.copy()
          scaledDx.assign(DoubleFunctions.mult(t))
          x.assign(scaledDx, DoubleFunctions.plus)
        }
      }
    }
    def xUpdateGradient() {
      def gradient(x: DoubleMatrix1D): DoubleMatrix1D = {
        val expTerm = new DenseDoubleMatrix1D(m)
        C.zMult(x, expTerm, 1.0, 1.0, false)
        expTerm.assign(DoubleFunctions.exp)
        val t1 = expTerm.copy()
        t1.assign(DoubleFunctions.plus(1.0)).assign(expTerm, DoubleFunctions.div).assign(DoubleFunctions.inv)
        val t2 = C.zMult(t1, null, 1.0, 1.0, true)
        val t3 = x.copy()
        t3.assign(z, DoubleFunctions.minus).assign(u, DoubleFunctions.plus).assign(DoubleFunctions.mult(rho))
        val grad = t2.copy()
        grad.assign(t3, DoubleFunctions.plus)
        grad
      }
      def loss(x: DoubleMatrix1D): Double = {
        val expTerm = new DenseDoubleMatrix1D(m)
        C.zMult(x, expTerm, 1.0, 1.0, false)
        expTerm.assign(DoubleFunctions.exp)
        val t1 = expTerm.copy()
        t1.assign(DoubleFunctions.plus(1.0)).assign(DoubleFunctions.log)
        val t2 = x.copy()
        t2.assign(z, DoubleFunctions.minus).assign(u, DoubleFunctions.plus)
        val norm = algebra.norm2(t2)
        norm * norm * rho / 2 + t1.zSum()
      }
      def backtrackingLineSearch(x: DoubleMatrix1D, dx: DoubleMatrix1D, alpha: Double, beta: Double): Double = {
        val fx = loss(x)
        val gradDot = -1.0*dx.zDotProduct(dx)
        def helper(t: Double): Double = {
          def rhs() = gradDot*alpha*t + fx
          def lhs() = {
            val temp = x.copy()
            temp.assign(dx,DoubleFunctions.plusMultSecond(t))
            loss(temp)
          }
          if (rhs() > lhs()) helper(beta*t) else t
        }
        helper(1.0)
      }
      def descent(x0: DoubleMatrix1D, maxIter: Int) {
        val alpha = .1
        val beta = .5
        for (_ <- 1 to maxIter) {
          val dx = gradient(x)
          dx.assign(DoubleFunctions.neg)
          val t = backtrackingLineSearch(x, dx, alpha, beta)
          x.assign(dx,DoubleFunctions.multSecond(t))
        }
      }
      descent(x,25)
    }
    def zUpdate() {
      z.assign(x).assign(u, DoubleFunctions.plus).assign(shrinkage)
    }
    def uUpdate() {
      u.assign(x, DoubleFunctions.plus).assign(z, DoubleFunctions.minus)
    }
    for (_ <- 1 to max_iter) {
      println(z.cardinality().toDouble/z.size())
      xUpdateGradient()
      zUpdate()
      uUpdate()
    }
    x
  }

  def main(args: Array[String]) {
    val nDocs = args(0).toInt
    val nSlices = 1
    val nFeatures = args(1).toInt
    SparseLogisticRegression.lambda = args(2).toDouble
    SparseLogisticRegression.rho = args(3).toDouble
    val A = rcv1IDF(nDocs, nSlices, nFeatures).head
    val b = labels(0, nDocs, nSlices).head
    val xEstimated = SparseLogisticRegression.solve(A, b)
    println(xEstimated)
  }
}
