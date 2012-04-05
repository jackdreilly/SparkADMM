package admm

/**
 * User: jdr
 * Date: 4/4/12
 * Time: 12:33 PM
 */


import cern.colt.matrix.impl._
import cern.colt.matrix.linalg.Algebra
import cern.jet.math.Functions
import cern.colt.matrix.{DoubleFactory1D, DoubleMatrix2D, DoubleFactory2D, DoubleMatrix1D}
import scala.util.control.Breaks._
import cern.colt.function.DoubleFunction
import data.RCV1Data.{labels, rcv1IDF}

object SparseLogisticRegression {
  var lambda = .3
  var rho = .01
  val algebra = new Algebra()
  val max_iter = 100
  val shrinkage = new DoubleFunction() {
    def apply(p1: Double): Double = {
      math.max(0, 1 - lambda / rho / math.abs(p1)) * p1
    }
  }

  def solve(A: SparseDoubleMatrix2D, b: SparseDoubleMatrix1D): DoubleMatrix1D = {
    val n = A.columns()
    val m = A.rows()
    val x = DoubleFactory1D.dense.make(n + 1, 0.0)
    val u = DoubleFactory1D.dense.make(n + 1, 0.0)
    val z = DoubleFactory1D.dense.make(n + 1, 0.0)
    val negA = A.copy()
    negA.assign(Functions.neg)
    val negb = DoubleFactory2D.sparse.make(m, 1)
    negb.viewColumn(0).assign(b).assign(Functions.neg)
    val C = DoubleFactory2D.sparse.appendColumns(negb, negA)

    def xUpdate() {
      def gradient(x: DoubleMatrix1D): DoubleMatrix1D = {
        val expTerm = new DenseDoubleMatrix1D(m)
        C.zMult(x, expTerm, 1.0, 1.0, false)
        expTerm.assign(Functions.exp)
        val t1 = expTerm.copy()
        t1.assign(Functions.plus(1.0)).assign(expTerm, Functions.div).assign(Functions.inv)
        val t2 = C.zMult(t1, null, 1.0, 1.0, true)
        val t3 = x.copy()
        t3.assign(z, Functions.minus).assign(u, Functions.plus).assign(Functions.mult(rho))
        val grad = t2.copy()
        grad.assign(t3, Functions.plus)
        grad
      }
      def hessian(x: DoubleMatrix1D): DoubleMatrix2D = {
        val expTerm = new DenseDoubleMatrix1D(m)
        C.zMult(x, expTerm, 1.0, 1.0, false)
        expTerm.assign(Functions.exp)
        val t1 = expTerm.copy()
        t1.assign(Functions.plus(1.0)).assign(Functions.square).assign(expTerm, Functions.div).assign(Functions.inv)
        val diag = DoubleFactory2D.dense.diagonal(t1)
        val tmp = C.zMult(diag, null, 1.0, 1.0, true, false)
        val sln = tmp.zMult(C, null, 1.0, 1.0, false, false)
        sln.assign(Functions.plus(rho))
        sln
      }
      def loss(x: DoubleMatrix1D): Double = {
        val expTerm = new DenseDoubleMatrix1D(m)
        C.zMult(x, expTerm, 1.0, 1.0, false)
        expTerm.assign(Functions.exp)
        val t1 = expTerm.copy()
        t1.assign(Functions.plus(1.0)).assign(Functions.log)
        val t2 = x.copy()
        t2.assign(z, Functions.minus).assign(u, Functions.plus)
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
            scaledDx.assign(Functions.mult(t))
            val diffX = x.copy()
            diffX.assign(scaledDx, Functions.plus)
            loss(diffX) > fx + alpha * t * dfx
          }) {
            t *= beta
          }
          val scaledDx = dx.copy()
          scaledDx.assign(Functions.mult(t))
          x.assign(scaledDx, Functions.plus)
        }
      }
    }
    def zUpdate() {
      z.assign(x).assign(u, Functions.plus).assign(shrinkage)
    }
    def uUpdate() {
      u.assign(x, Functions.plus).assign(z, Functions.minus)
    }
    for (_ <- 1 to max_iter) {
      println(x)
      xUpdate()
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
