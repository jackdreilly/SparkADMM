package admm


import spark.SparkContext
import cern.colt.matrix.tdouble.{DoubleMatrix1D, DoubleFactory1D}


/**
 * Created by IntelliJ IDEA.
 * User: Jojo
 * Date: 23/04/12
 * Time: 10:03
 * To change this template use File | Settings | File Templates.
 */

object TestSLRDistSpark {


  def main(args: Array[String]) {
    val sc = new SparkContext("local[2]", "TestSLRDist")
    val v1 = DoubleFactory1D.dense.make(2)
    val v2 = DoubleFactory1D.dense.make(2)
    val v3 = DoubleFactory1D.dense.make(2)
    val v4 = DoubleFactory1D.dense.make(2)
    val v5 = DoubleFactory1D.dense.make(2)

    v1.setQuick(0,1)
    v2.setQuick(0,2)
    v3.setQuick(0,3)
    v4.setQuick(0,4)
    v5.setQuick(0,5)

    val data = Array(v1,v2,v3,v4,v5)
    data.foreach(v => v.setQuick(1,v.getQuick(0)+1))
    data.toArray.foreach(println)
    val distData =  sc.parallelize(data)

   /* val a = DoubleFactory1D.dense.make(1)
    a.assign(2)
    println(a) */

    def modif (v: DoubleMatrix1D ) { v.setQuick(1,10)}
    modif(v1)
    println("v1 after update:")
    println(v1)
    distData.foreach(
    data => modif(data)
    )
    distData.toArray().foreach(println)
  }

}
