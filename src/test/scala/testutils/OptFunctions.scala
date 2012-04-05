package testutils

/**
 * User: jdr
 * Date: 3/19/12
 * Time: 12:04 AM
 */

import org.scalatest.{FunSuite, BeforeAndAfter}
import utils.OptFunctions._
import scalala.tensor.dense.{DenseVector, DenseMatrix}

class OptFunctionsSuite extends FunSuite with BeforeAndAfter {
  val kappa = .5
  val kThresh = softThreshold(kappa)
  val kNeg = -kappa
  val kHalf = kappa / 2
  val kNegHalf = kNeg / 2
  val kDouble = kappa * 2
  val kNegDouble = kNeg * 2
  val nRows = 20
  val nCols = 5
  val mat = DenseMatrix.rand(nRows, nCols)
  val vec = DenseVector.rand(nRows)

  test("soft threshold should be zero when abs val < threshold") {
    assert(kThresh(kHalf) === 0)
    assert(kThresh(kNegHalf) === 0)
  }
  test("s.t. should subtract off kappa from abs value, but preserve sign") {
    assert(kThresh(kDouble) === kDouble - kappa)
    assert(kThresh(kNegDouble) === kNegDouble + kappa)
  }
  test("duplicator should have same value repeated x times") {
    val size = 10
    val duplicated = duplicate(kappa, size)
    duplicated.foreach(current => assert(current === kappa))
    assert(duplicated.size === size)
  }
  test("matrix slice should have x slices of same width") {
    val nSlices = 5
    val sliced = sliceMatrix(mat, nSlices)
    sliced.foreach(m => {
      assert(m.numCols === nCols)
      assert(m.numRows === nRows / nSlices)
    })
    assert(sliced.length === nSlices)
  }
  test("cant slice matrix without clean divide") {
    intercept[AssertionError] {
      val slices = sliceMatrix(mat, 3)
    }
  }
  test("vec slice should have x slices of same size") {
    val nSlices = 5
    val sliced = sliceVector(vec, nSlices)
    sliced.foreach(v => {
      assert(v.size === nRows / nSlices)
    })
    assert(sliced.length === nSlices)
  }
  test("cant slice vector without clean divide") {
    intercept[AssertionError] {
      val slices = sliceVector(vec, 3)
    }
  }


}

