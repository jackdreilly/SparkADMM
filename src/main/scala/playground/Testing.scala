package playground

/**
 * User: jdr
 * Date: 3/27/12
 * Time: 4:43 PM
 */

import cern.colt.matrix.impl.DenseDoubleMatrix1D
import cern.colt.matrix.linalg.SmpBlas.smpBlas


object Testing extends App {
  val x = new DenseDoubleMatrix1D(5)
  val y = new DenseDoubleMatrix1D(5)
  val alg = smpBlas
  x.setQuick(3, 2.0)
  smpBlas.daxpy(2.0, x, y)
  println(y)

}