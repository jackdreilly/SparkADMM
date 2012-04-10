package admm

/**
 * Created by IntelliJ IDEA.
 * User: Jojo
 * Date: 08/04/12
 * Time: 04:44
 * To change this template use File | Settings | File Templates.
 */


import cern.colt.matrix.tdouble.impl.{SparseDoubleMatrix1D, SparseDoubleMatrix2D}
import cern.colt.matrix.tdouble.{DoubleFactory1D, DoubleFactory2D}
import scala.util.Random
import cern.jet.math.tdouble.{DoubleFunctions}
import cern.colt.list
import list.tdouble.DoubleArrayList
import list.tint.IntArrayList

object testLogRADMM {
  def main(args: Array[String]) {
    val nDocs = 20
    val nFeatures = 10

    // Calculation of bTrue

    val w = DoubleFactory1D.sparse.random(nFeatures)
    w.assign(DoubleFunctions.minus(0.5))
    val v = Random.nextGaussian()
    val V = DoubleFactory1D.sparse.make(nDocs,v)
    val X = DoubleFactory2D.sparse.random(nDocs,nFeatures)
    X.assign(DoubleFunctions.minus(0.5))

    val bTrue = new SparseDoubleMatrix1D(nDocs)
    X.zMult(w,bTrue)
    bTrue.assign(V, DoubleFunctions.plus)
    bTrue.assign(DoubleFunctions.sign)

    //Calculation of A

    val B = DoubleFactory2D.sparse.diagonal(bTrue)
    val A = new SparseDoubleMatrix2D(nDocs,nFeatures)
    B.zMult(X,A)

    // Calculation of ratio

    val bRatio = new SparseDoubleMatrix1D(nDocs)
    bRatio.assign(bTrue)
    var ratio = bRatio.assign(DoubleFunctions.plus(1)).zSum()
    ratio /= 2*nDocs

    // Calculation of mu

      // Calculation of sum(A(bTrue==1,:),1) and sum(A(bTrue==-1,:),1) first

    val posIndex = new IntArrayList(nDocs)
    val negIndex = new IntArrayList(nDocs)
    val posVal = new DoubleArrayList(nDocs)
    val negVal = new DoubleArrayList(nDocs)
    bTrue.getPositiveValues(posIndex,posVal)
    bTrue.getNegativeValues(negIndex,negVal)

    val pos = new Array[Int](posIndex.size())
    var j = 0
    for (i <- 0 to (posIndex.size()-1)){
      j = posIndex.getQuick(i)
      pos(i) = j
    }
    val neg = new Array[Int](negIndex.size())
    var k = 0
    for (i <- 0 to (negIndex.size()-1)){
      k = negIndex.getQuick(i)
      neg(i) = k
    }
    
    val row = new Array[Int](nFeatures)
    for (i <- 0 to (nFeatures-1)){
      row(i) = i
    }

    val posSum = new SparseDoubleMatrix1D(nFeatures)
    val onesPos = new SparseDoubleMatrix1D(A.viewSelection( pos, row).rows())
    onesPos.assign(1)
    A.viewSelection( pos, row).zMult(onesPos,posSum,1.00,1.00,true)

    val negSum = new SparseDoubleMatrix1D(nFeatures)
    val onesNeg = new SparseDoubleMatrix1D(A.viewSelection( neg, row).rows())
    onesNeg.assign(1)
    A.viewSelection( neg, row).zMult(onesNeg,negSum,1.00,1.00,true)

      // Calculation of (1-ratio)*sum(A(bTrue==1,:),1) + ratio*sum(A(bTrue==-1,:),1)

    posSum.assign(DoubleFunctions.mult(1-ratio))
    negSum.assign(DoubleFunctions.mult(ratio))

    posSum.assign(negSum,DoubleFunctions.plus)

      // Calculation of norm(..., inf) and then mu

    posSum.assign(DoubleFunctions.abs)

    var mu = posSum.getQuick(0)
    
    for (j <- 1 to (nFeatures - 1)){
      mu = mu.max(posSum.getQuick(j))
    }

    mu  *= 0.1
    mu  /= nDocs
  }

}
