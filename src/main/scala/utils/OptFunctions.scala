package utils


import OptTypes.Vec
import scala.math.min


/**
 * Created by IntelliJ IDEA.
 * User: jdr
 * Date: 3/15/12
 * Time: 9:19 PM
 * To change this template use File | Settings | File Templates.
 */

object OptFunctions {
  def softThreshold(kappa: Double): (Vec) => Vec = {
    def softKappa(vec: Vec): Vec= {
      vec.map{(param: Double) => min(1 - kappa/param, 0)*param}
    }
    softKappa
  }

}
