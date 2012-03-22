package data

/**
 * User: jdr
 * Date: 3/20/12
 * Time: 8:06 PM
 */
import scala.io._
import cern.colt.matrix.impl.SparseDoubleMatrix2D

object RCV1Data  {
  def main(args: Array[String]) {
    val s = Source.fromFile("/Users/jdr/Downloads/topic_hits.admm.data")
    val labels = s.getLines().next().split(" ").map(_.toInt)
    val t = Source.fromFile("/Users/jdr/Downloads/word_counts.admm.data")
    val docs = for (line <- t.getLines()) yield {
      for (pair <- line.split(":")) yield {
        val splits = pair.split(", ")
        val front = splits(0)
        val back = splits(1)
        val feature = front.substring(1,front.length()).toInt
        val weight = back.substring(0,back.length() - 1).toDouble
        (feature, weight)
      }
    }
    val da = docs.toList
    val nFeatures = 48000
    val nSamples =  da.size
    val data = new SparseDoubleMatrix2D(nSamples, nFeatures)
    for ((array, docIndex)<- da.zipWithIndex) {
      for ((feature, weight) <- array) {
        data.set(docIndex, feature,weight)
      }
    }

    println(data.get(0,25503))

  }



}
