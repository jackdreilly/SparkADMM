package admmutils

import scalala.tensor.dense.{DenseVectorCol, DenseMatrix}
import cern.colt.matrix.impl.{SparseDoubleMatrix1D}


/**
 * User: jdr
 * Date: 3/15/12
 * Time: 10:40 PM
 */

object OptTypes {
  type Vec = DenseVectorCol[Double]
  type Mat = DenseMatrix[Double]
  type UpdateFn = (Mat, Mat, Vec, Double, Vec, Vec, Vec) => Vec
  type VecSp = SparseDoubleMatrix1D
}

