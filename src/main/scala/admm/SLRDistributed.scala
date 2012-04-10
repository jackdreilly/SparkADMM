package admm

import admmutils.ADMMFunctions
import cern.jet.math.tdouble.DoubleFunctions
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.{DoubleFactory2D, DoubleMatrix1D, DoubleFactory1D}
import data.{SlicedDataSet, DataSet}
import data.RCV1Data.{SampleSet, OutputSet, getDataset}
import scala.util.control.Breaks._


/**
 * User: jdr
 * Date: 4/9/12
 * Time: 9:39 PM
 */

object SLRDistributed {
  type Vector = DoubleMatrix1D
  val algebra = new DenseDoubleAlgebra()
  case class MapEnvironment(A: SampleSet, b: OutputSet, x: Vector, u: Vector, z: Vector ) {
    val bPrime = b.copy()
    bPrime.assign(DoubleFunctions.mult(2.0)).assign(DoubleFunctions.minus(1.0))
    val C = DoubleFactory2D.sparse.appendColumn(A,bPrime)
    C.assign(DoubleFunctions.neg)
    val m = A.rows()
    val n = A.columns()
    def xUpdate() {
      def gradient(x:Vector): Vector = {
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
      def loss(x: Vector): Double = {
        val expTerm = C.zMult(x,null)
        expTerm.assign(DoubleFunctions.exp)
          .assign(DoubleFunctions.plus(1.0))
          .assign(DoubleFunctions.log)
        val normTerm = x.copy()
        normTerm.assign(z,DoubleFunctions.minus)
          .assign(u,DoubleFunctions.plus)
        expTerm.zSum() + math.pow(algebra.norm2(normTerm),2)*rho/2
      }
      def backtracking(x: Vector, dx: Vector, grad: Vector): Double = {
        val t0 = 1.0
        val alpha = .1
        val beta = .5
        val lossX = loss(x)
        val rhsCacheTerm = dx.zDotProduct(grad)*alpha
        def lhs(t: Double): Double = {
          val newX = dx.copy()
          newX.assign(DoubleFunctions.mult(t)).assign(x,DoubleFunctions.plus)
          loss(newX)
        }
        def rhs(t: Double): Double = {
          lossX + t*rhsCacheTerm
        }
        def helper(t: Double): Double = {
          if (lhs(t) > rhs(t)) helper(beta*t) else t
        }
        helper(t0)
      }

      def descent(x0: DoubleMatrix1D, maxIter: Int): Vector = {
        val tol = 1e-4
        breakable {for (i <- 1 to maxIter) {
          val dx = gradient(x0)
          dx.assign(DoubleFunctions.neg)
          val t = backtracking(x,dx,gradient(x0))
          x0.assign(dx,DoubleFunctions.multSecond(t))
          if (algebra.norm2(dx) < tol) break()
        }
        }
        x0
      }
      x.assign(descent(DoubleFactory1D.dense.make(x.size().toInt),25))
    }
    def uUpdate() {
      u.assign(x,DoubleFunctions.plus)
        .assign(z,DoubleFunctions.minus)
    }
  }
  var maxIter = 100
  var rho = 1.0
  var lambda = 2.0

  def solve(data: DataSet[SampleSet,OutputSet]) {
    data match {
      case SlicedDataSet(slices) => {
        val nDocsPerSlice = slices.head.samples.rows
        val nFeatures = slices.head.samples.columns
        val nSlices = slices.size
        val z: Vector = DoubleFactory1D.dense.make(nFeatures+1)
        val environments = slices.map{  slice =>
          val x: Vector = DoubleFactory1D.dense.make(nFeatures+1)
          val u: Vector = DoubleFactory1D.dense.make(nFeatures+1)
          MapEnvironment(slice.samples, slice.output, x,u,z)
        }
        val xs = environments.map{_.x}
        val us = environments.map{_.u}
        val zUpdate = () => {
          z.assign(ADMMFunctions.mean(xs))
            .assign(ADMMFunctions.mean(us),DoubleFunctions.plus)
            .assign(ADMMFunctions.shrinkage(lambda/rho/nSlices.toDouble))
        }
        for (_ <- 1 to maxIter) {

          environments.foreach{_.xUpdate()}
          println("x update")
          environments.foreach{env => println(algebra.norm2(env.x)) }
          zUpdate()
          println("z update")
          println(algebra.norm2(z))
          environments.foreach{_.uUpdate()}
          println("u update")
          environments.foreach{env => println(algebra.norm2(env.u))  }
        }
      }
    }
  }
  def main(args: Array[String]) {
    val nDocs = args(0).toInt
    val nFeatures = args(1).toInt
    val docIndex = args(2).toInt
    val nSlices = args(3).toInt
    lambda = args(4).toDouble
    rho = args(5).toDouble
    maxIter = args(6).toInt
    solve(getDataset(nDocs,nFeatures,docIndex,nSlices))
  }
}


