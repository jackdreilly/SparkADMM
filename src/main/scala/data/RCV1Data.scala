package data

/**
 * User: jdr
 * Date: 3/20/12
 * Time: 8:06 PM
 */
import scala.io._
import cern.colt.matrix.impl.SparseDoubleMatrix2D

object RCV1Data  {
  def rcv1IDF(): SparseDoubleMatrix2D =  {
    val nSamples = 23149
    val nFeatures = 47236
    val data = new SparseDoubleMatrix2D(nSamples,nFeatures)
    for ((line, docIndex)<- Source.fromFile("etc/data/word_counts.admm.data").getLines().zipWithIndex) {
      for (pair <- line.split(":")) {
        val splits = pair.split(", ")
        val front = splits(0)
        val back = splits(1)
        val feature = front.substring(1,front.length()).toInt
        val weight = back.substring(0,back.length() - 1).toDouble
        data.setQuick(docIndex,feature,weight)
      }
    }
    data
  }

  val labels = (index: Int) => {
    val s = Source.fromFile("etc/data/topic_hits.admm.data")
    s.getLines().drop(index - 1).next().split(" ").map(_.toInt)
  }
  
  def main(args: Array[String]) {
    val data = rcv1IDF()
    val labs = labels(0)
  }



}
