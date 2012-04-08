package admm

/**
 * Created by IntelliJ IDEA.
 * User: Jojo
 * Date: 07/04/12
 * Time: 23:57
 * To change this template use File | Settings | File Templates.
 */

import data.RCV1Data

object testRCV1Data {
  def main(args: Array[String]) {
    // val nSlices = 1
    // val nDocs = args(0).toInt
    // val nFeatures = args(1).toInt
    val nDocs = 60
    val nFeatures = 900
    val A = RCV1Data.rcv1IDF(nDocs,3,nFeatures)
    println("A")
    println(A)
  }
}
