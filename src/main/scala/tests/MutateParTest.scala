package tests

import spark.SparkContext
import tests.MutateParTest.Junk

/**
 * User: jdr
 * Date: 4/23/12
 * Time: 5:14 PM
 */

object MutateParTest extends App {

  val sc = new SparkContext("local", "jack")

  class Junk extends Serializable {
    var x = 1
  }

  val junks = (1 to 10).map(i => new Junk)

  val junksPar = sc.parallelize(junks).cache()

  junksPar.foreach(junk => println(junk.x))
  junksPar.foreach(junk => junk.x = 2)
  junksPar.foreach(junk => println(junk.x))

}
