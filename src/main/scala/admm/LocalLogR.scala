package admm

/**
 * Created by IntelliJ IDEA.
 * User: Boris
 * Date: 20/03/12
 * Time: 22:16
 * To change this template use File | Settings | File Templates.
 */

import java.util.Random
import utils.VectorBis

object LocalLogR {
  val N = 10000  // Number of data points
  val D = 10   // Number of dimensions
  val R = 0.7  // Scaling factor
  val ITERATIONS = 5
  val rand = new Random(42)

  case class DataPoint(x: VectorBis, y: Double)

  def generateData = {
    def generatePoint(i: Int) = {
      val y = if(i % 2 == 0) -1 else 1
      val x = VectorBis(D, _ => rand.nextGaussian + y * R)
      DataPoint(x, y)
    }
    Array.tabulate(N)(generatePoint)
  }

  def main(args: Array[String]) {
    val data = generateData

    // Initialize w to a random value
    var w = VectorBis(D, _ => 2 * rand.nextDouble - 1)
    println("Initial w: " + w)

    for (i <- 1 to ITERATIONS) {
      println("On iteration " + i)
      var gradient = VectorBis.zeros(D)
      for (p <- data) {
        val scale = (1 / (1 + math.exp(-p.y * (w dot p.x))) - 1) * p.y
        gradient +=  scale * p.x
      }
      w -= gradient
    }

    println("Final w: " + w)
  }

}
