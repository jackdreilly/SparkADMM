package admm

import utils.NoisyData

/**
 * Created by IntelliJ IDEA.
 * User: jdr
 * Date: 3/17/12
 * Time: 4:02 PM
 * To change this template use File | Settings | File Templates.
 */

object LocalLasso {
  def main(args: Array[String]) {
    val nSamples = 1000
    val nFeatures = 10
    val A = NoisyData.genData(nSamples,nFeatures)
    val trueFeatures = NoisyData.genState(nFeatures)
    val noisyOutput = NoisyData.genOutput(A,noisyOutput)

  }
}
