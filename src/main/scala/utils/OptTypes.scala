package utils

import scalala.tensor.dense.{DenseVectorCol, DenseMatrix}


/**
 * Created by IntelliJ IDEA.
 * User: jdr
 * Date: 3/15/12
 * Time: 10:40 PM
 * To change this template use File | Settings | File Templates.
 */

object OptTypes {
  type Vec = DenseVectorCol[Double]
  type Matrix = DenseMatrix[Double]
}
