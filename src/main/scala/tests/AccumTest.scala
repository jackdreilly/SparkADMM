package tests

import spark.SparkContext
import cern.colt.matrix.tdouble.DoubleFactory1D
import cern.jet.math.tdouble.DoubleFunctions
import admm.Vector

/**
 * User: jdr
 * Date: 4/23/12
 * Time: 5:54 PM
 */

object AccumTest extends App {
  val sc = new SparkContext("local", "jack")
  val envs = (1 to 2).map(i => DoubleFactory1D.sparse.make(2,2.0))
  val z = DoubleFactory1D.sparse.make(2)
  val parEnvs = sc.parallelize(envs).cache()
  for (_ <- 1 to 2) {
    println("new iter")
    val accum = sc.accumulator(Vector(z.toArray))
    parEnvs.foreach(env => {
      env.assign(z,DoubleFunctions.plus)
      accum+=Vector(env.toArray)
    }
    )
    z.assign(accum.value.elements)
    println("envs")
    parEnvs.foreach(println)
    println("accum")
    println(accum)
    println("z")
    println(z)
  }
}
