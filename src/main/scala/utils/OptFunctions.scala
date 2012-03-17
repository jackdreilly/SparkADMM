package utils


import OptTypes.Vec
import scala.math.max
import scalala.library.Library.linspace
import scalala.library.Plotting._


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
      vec.map{(param: Double) => max(1 - kappa/param.abs, 0)*param}
    }
    softKappa
  }

  def main(args: Array[String]) {
    val kappa = 3.
    val kappaThreshold = softThreshold(kappa)
    val tVals = linspace(-5,5,100)
    plot(tVals,kappaThreshold(tVals))

  }

}
