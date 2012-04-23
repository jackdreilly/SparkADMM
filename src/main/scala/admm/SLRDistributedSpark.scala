package admm

import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra

import cern.jet.math.tdouble.DoubleFunctions
import util.control.Breaks._
import data.{SlicedDataSet, SingleSet, DataSet}
import spark.SparkContext
import SparkContext._
import data.ReutersData._
import cern.colt.matrix.tdouble.{DoubleMatrix1D, DoubleFactory1D, DoubleFactory2D}
import Vector._


/**
 * Created by IntelliJ IDEA.
 * User: Jojo
 * Date: 19/04/12
 * Time: 02:51
 * To change this template use File | Settings | File Templates.
 */

object SLRDistributedSpark {

  val printStuff = false
  var counter = 0
  //type Vector = DoubleMatrix1D
  val algebra = new DenseDoubleAlgebra()
  val alpha = 1000.0


  def xUpdate(A: SampleSet, b: OutputSet, x: DoubleMatrix1D, u: DoubleMatrix1D, z: DoubleMatrix1D ):  (SampleSet, OutputSet, DoubleMatrix1D, DoubleMatrix1D) = {

    //val z = DoubleFactory1D.dense.make(oldZ.elements)

    val bPrime = b.copy()
    bPrime.assign(DoubleFunctions.mult(2.0)).assign(DoubleFunctions.minus(1.0)).assign(DoubleFunctions.mult(alpha))
    val Aprime = DoubleFactory2D.sparse.diagonal(bPrime).zMult(A, null)
    val C = DoubleFactory2D.sparse.appendColumns(bPrime.reshape(bPrime.size().toInt, 1), Aprime)
    //val C = DoubleFactory2D.sparse.appendColumns(bPrime.reshape(bPrime.size().toInt,1),A)
    C.assign(DoubleFunctions.neg)
    val m = A.rows()
    val n = A.columns()

    def loss(x: DoubleMatrix1D): Double = {
      val expTerm = C.zMult(x, null)
      expTerm.assign(DoubleFunctions.exp)
        .assign(DoubleFunctions.plus(1.0))
        .assign(DoubleFunctions.log)
      val normTerm = x.copy()
      normTerm.assign(z, DoubleFunctions.minus)
        .assign(u, DoubleFunctions.plus)
      expTerm.zSum() + math.pow(algebra.norm2(normTerm), 2) * rho / 2
    }

    def gradient(x: DoubleMatrix1D): DoubleMatrix1D = {
      val expTerm = C.zMult(x, null)
      expTerm.assign(DoubleFunctions.exp)
      val firstTerm = expTerm.copy()
      firstTerm.assign(DoubleFunctions.plus(1.0))
        .assign(DoubleFunctions.inv)
        .assign(expTerm, DoubleFunctions.mult)
      val secondTerm = x.copy()
      secondTerm.assign(z, DoubleFunctions.minus)
        .assign(u, DoubleFunctions.plus)
        .assign(DoubleFunctions.mult(rho))
      val returnValue = C.zMult(firstTerm, null, 1.0, 1.0, true)
      returnValue.assign(secondTerm, DoubleFunctions.plus)
      returnValue
    }
    /*def loss(x: DoubleMatrix1D): Double = {
      val expTerm = C.zMult(x, null)
      expTerm.assign(DoubleFunctions.exp)
        .assign(DoubleFunctions.plus(1.0))
        .assign(DoubleFunctions.log)
      val normTerm = x.copy()
      normTerm.assign(z, DoubleFunctions.minus)
        .assign(u, DoubleFunctions.plus)
      expTerm.zSum() + math.pow(algebra.norm2(normTerm), 2) * rho / 2
    } */
    def backtracking(x: DoubleMatrix1D, dx: DoubleMatrix1D, grad: DoubleMatrix1D): Double = {
      val t0 = 1.0
      val alpha = .1
      val beta = .5
      val lossX = loss(x)
      val rhsCacheTerm = dx.zDotProduct(grad) * alpha
      def lhs(t: Double): Double = {
        val newX = x.copy()
        newX.assign(dx, DoubleFunctions.plusMultSecond(t))
        loss(newX)
      }
      def rhs(t: Double): Double = {
        lossX + t * rhsCacheTerm
      }
      def helper(t: Double): Double = {
        if (lhs(t) > rhs(t)) helper(beta * t) else t
      }
      helper(t0)
    }

    def descent(x0: DoubleMatrix1D, maxIter: Int): DoubleMatrix1D = {
      val tol = 1e-4
      breakable {
        for (i <- 1 to maxIter) {
          val dx = gradient(x0)
          dx.assign(DoubleFunctions.neg)
          val t = backtracking(x, dx, gradient(x0))
          x0.assign(dx, DoubleFunctions.plusMultSecond(t))
          if (algebra.norm2(dx) < tol) break()
        }
      }
      x0
    }
    x.assign(descent(x, 25))

    (A, b, x, u)

  }

  var maxIter = 100
  var rho = 1.0
  var lambda = 2.0

  def uUpdate(sample : SampleSet, output: OutputSet, x: DoubleMatrix1D, u: DoubleMatrix1D, z: DoubleMatrix1D ) : (SampleSet,OutputSet,DoubleMatrix1D,DoubleMatrix1D) = {
   // val z =  DoubleFactory1D.dense.make(oldZ.elements)
    u.assign(x,DoubleFunctions.plus)
      .assign(z,DoubleFunctions.minus)

    (sample,output,x,u)
  }

  def addXU (data: (SampleSet,OutputSet), nFeatures : Int) : (SampleSet,OutputSet,DoubleMatrix1D,DoubleMatrix1D) = {
    
    val x: DoubleMatrix1D = DoubleFactory1D.dense.make(nFeatures+1)
    val u: DoubleMatrix1D = DoubleFactory1D.dense.make(nFeatures+1)
    
    (data._1,data._2,x,u)
  }

  def main(args: Array[String]) {

    val sc = new SparkContext("local[2]", "SLRDist")
    val nDocs = args(0).toInt
    val nFeatures = args(1).toInt
    val docIndex = args(2).toInt
    val nSlices = args(3).toInt
    lambda = args(4).toDouble
    rho = args(5).toDouble
    maxIter = args(6).toInt
    val distData = ReutersRDD.localTextRDD(sc, "etc/data/labeled_rcv1.admm.data").splitSets(nSlices)

    
    val z: DoubleMatrix1D = DoubleFactory1D.dense.make(nFeatures+1)
    var broadcastZ = sc.broadcast(z)
    var distDataXU = distData.map {
     data => addXU(data.generateReutersSet(0),nFeatures)
    }
    
    val xMean = DoubleFactory1D.dense.make(nFeatures+1)
    val uMean = DoubleFactory1D.dense.make(nFeatures+1)
    
    for (_ <- 1 to maxIter) {
      val accumX = sc.accumulator(Vector.zeros(nFeatures+1))
      val accumU = sc.accumulator(Vector.zeros(nFeatures+1))

      distDataXU = distDataXU.map {
        data => {
          val newData  = xUpdate(data._1, data._2, data._3, data._4, broadcastZ.value)
          val newX = Vector(newData._3.toArray)
          val newU = Vector(newData._4.toArray)
          accumX += newX
          accumU += newU

          newData
        }
      }

      xMean.assign(accumX.value.elements.map (_ / nSlices))
      uMean.assign(accumU.value.elements.map (_ / nSlices))
      z.assign(xMean,DoubleFunctions.plus).assign(uMean,DoubleFunctions.plus).assign(DoubleFunctions.mult(lambda / rho / nSlices))
      broadcastZ = sc.broadcast(z)
      distDataXU = distDataXU.map {
        data => uUpdate(data._1, data._2, data._3, data._4, broadcastZ.value )
      }
    }
     println(z.viewPart(1,nFeatures))
    
   /* val x = z.viewPart(1,nFeatures)
    val goodslices = data match {
      case SlicedDataSet(slices) => {
        slices.map{
          case SingleSet(a,b) => {
            a.toArray.zip(b.toArray).filter{
              case (ai,bi) => bi > .5
            }.map{case (ai,bi) => DoubleFactory1D.sparse.make(ai).zDotProduct(x)}
          }
        }.flatten
      }
    }
    val badSlices = data match {
      case SlicedDataSet(slices) => {
        slices.map{
          case SingleSet(a,b) => {
            a.toArray.zip(b.toArray).filter{
              case (ai,bi) => bi < .5
            }.map{case (ai,bi) => DoubleFactory1D.sparse.make(ai).zDotProduct(x)}
          }
        }.flatten
      }
    }
    val vwish = -.5*(goodslices.reduce{_+_}/goodslices.size + badSlices.reduce{_+_}/badSlices.size)
    val vreal = xEst.getQuick(0)
    val vs = List(vreal,vwish)
    vs.map{v =>{
      def loss(mu: Double): Double = math.log(1 + math.exp(-mu))
      def mu(ai: DoubleMatrix1D, bi: Double): Double = (2*bi - 1)*(ai.zDotProduct(x) + v)
      val totalLoss = data match {
        case SlicedDataSet(slices) => {
          slices.map{
            case SingleSet(as,bs) => {
              as.toArray.zip(bs.toArray).map{case (ai,bi) =>{
                loss(mu(DoubleFactory1D.dense.make(ai),bi))
              }}
            }
          }.flatten.reduce{_+_}
        }
      }
      println(totalLoss)
    }}
    val goodavg = goodslices.reduce{_+_}/goodslices.size
    val badavg = badSlices.reduce{_+_}/badSlices.size
    val v = vreal


    data match {
      case SlicedDataSet(slices) => {
        var onegood = 0
        var onetotal = 0
        var zerogood = 0
        var zerototal = 0
        slices.foreach{slice =>{
          val A = slice.samples
          val b = slice.output

          (0 until A.rows()).map{A.viewRow(_)}.zip(b.toArray).foreach{case (ai,bi) =>{
            val biprime = bi*2 - 1
            val mu = biprime*(x.zDotProduct(ai) + v)
            bi match {
              case 0 => {zerototal+=1}
              case 1 => {onetotal+=1}
            }
            mu > 0 match {
              case true => {
                bi match {
                  case 0 => {zerogood+=1}
                  case 1 => {onegood+=1}
                }
              }
              case _ => {}
            }
          }
          }}

        }
        println("positive success: " + (onegood.toDouble/onetotal).toString)
        println("negative success: " + (zerogood.toDouble/zerototal).toString)
        println(v)
        println(goodavg )
        println(badavg)
        println(goodslices.map{a => math.pow(a-goodavg,2)}.reduce{_+_}/goodslices.size)
        println(badSlices.map{a => math.pow(a-badavg,2)}.reduce{_+_}/badSlices.size)
        println(goodslices.size.toDouble/(goodslices.size + badSlices.size))
      }
    }*/
  }
}
