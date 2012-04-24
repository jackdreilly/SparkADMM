package tests


import spark.{RDD, SparkContext}
import cern.colt.matrix.tdouble.{DoubleFactory1D, DoubleMatrix1D}

/**
 * User: jdr
 * Date: 4/23/12
 * Time: 5:14 PM
 */

object MutateParTest {
  def simpleTest() {
    // set up stuff
    // create spark context
    // create mutable class
    val sc = new SparkContext("local", "jack")
    class Junk extends Serializable {
      var x = 1
    }

    // general test
    def testMutation(rdd: RDD[Junk], testName: String) {
      println("testing " +  testName + "\n##########\n########")
      println("before mutate\n\n\n")
      rdd.foreach(item => println(item.x))
      rdd.foreach(item => item.x = 2)
      println("after mutate\n\n\n")
      rdd.foreach(item => println(item.x))
    }
    // test without caching
    // this doesn't work because local variables are serialized and then dumped after each mapping
    def testWithoutCache {
      val rdd = sc.parallelize((1 to 3).map(i => new Junk))
      testMutation(rdd, "No Cache")
    }
    // test with caching
    // this works because a local variable is permanently stored in memory across mappings
    // therefore when the print loop occurs, the reference to the mutated version is kept
    def testWithCache {
      val rdd = sc.parallelize((1 to 3).map(i => new Junk)).cache()
      testMutation(rdd, "With Cache")
    }

    testWithoutCache
    testWithCache

  }

  def vectorTest() {
    val sc = new SparkContext("local", "jack")
    class ExtraVector(vec1: DoubleMatrix1D) extends Serializable {
      val vec2 = DoubleFactory1D.sparse.make(2)
      def getVec1 = vec1
    }
    val cached = sc.parallelize(
    (1 to 2).map(i => new ExtraVector(DoubleFactory1D.sparse.make(1)))
    ).cache()
    cached.foreach(vecs => println(vecs.vec2))
    cached.foreach(vecs => vecs.vec2.setQuick(1,1))
    cached.foreach(vecs => println(vecs.vec2))
  }
  def main(args: Array[String]) {
    vectorTest()
  }


}
