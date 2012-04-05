package data

/**
 * User: jdr
 * Date: 3/20/12
 * Time: 8:06 PM
 */

import scala.io._
import cern.colt.matrix.impl.{SparseDoubleMatrix2D, SparseDoubleMatrix1D}
import admmutils.ListHelper.list2helper

object RCV1Data {
  def rcv1IDF(nDocs: Int = 23149, nSlices: Int = 1, nFeatures: Int = 47236): List[SparseDoubleMatrix2D] = {
    val docsPerSlice = nDocs / nSlices
    val sliceGroups = Source.fromFile("etc/data/word_counts.admm.data")
      .getLines
      .take(nDocs)
      .zipWithIndex
      .toList
      .chunk(docsPerSlice)
    val slices = sliceGroups.map(slice => {
      val data = new SparseDoubleMatrix2D(docsPerSlice, nFeatures)
      for ((line, docIndex) <- slice) {
        for (pair <- line.split(":")) {
          val splits = pair.split(", ")
          val front = splits(0)
          val back = splits(1)
          val feature = front.substring(1, front.length()).toInt
          val weight = back.substring(0, back.length() - 1).toDouble
          data.setQuick(docIndex, feature, weight)
        }
      }
      data
    })
    slices
  }

  def labels(index: Int = 0, nDocs: Int = 23149, nSlices: Int = 1): List[SparseDoubleMatrix1D] = {
    val s = Source.fromFile("etc/data/topic_hits_transpose.admm.data")
    val docsPerSlice = nDocs / nSlices
    s.getLines
      .drop(index - 1)
      .next
      .split(" ")
      .take(nDocs)
      .map(_.toInt)
      .toList
      .chunk(docsPerSlice)
      .map(list => {
      val vector = new SparseDoubleMatrix1D(docsPerSlice)
      list.zipWithIndex.filter {
        _._1 > 0
      }.foreach {
        case (label, ind) => vector.setQuick(ind, label)
      }
      vector
    })
  }

  def main(args: Array[String]) {
    val nSlices = 1000
    val nDocs = 10000
    val nFeatures = 5000
    val data = rcv1IDF(nDocs, nSlices, nFeatures).toList
    val labs = labels(0, nDocs, nSlices)
  }


}
