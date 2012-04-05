package testutils

/**
 * User: jdr
 * Date: 3/19/12
 * Time: 12:42 AM
 */

import admmutils.NoisyData
import org.scalatest.FunSuite
import scalala.library.Library.linspace
import admmutils.OptTypes._

class NoisySuite extends FunSuite {

  test("if you want it sparse it should be sparse most of the time") {
    val sparsities = linspace(.1, .9, 10)
    val nRuns = 1000
    val nFeatures = 1000
    val successBuffer = .1
    def passingSparsity(vec: Vec, sparsity: Double): Boolean = {
      vec.filter(_ == 0.0).size >= vec.length * (1 - sparsity)
    }
    sparsities.foreach(lambda => {
      val vectors = for (_ <- 1 to nRuns) yield NoisyData.genSparseState(nFeatures, lambda)
      val passingVectors = vectors.filter(passingSparsity(_, lambda))
      val passingRate = (passingVectors.size).toDouble / nRuns
      println(passingRate)
      assert(passingRate >=.5 - successBuffer)
    })
  }


}
