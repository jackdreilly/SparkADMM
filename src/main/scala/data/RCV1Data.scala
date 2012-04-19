package data

/**
 * User: jdr
 * Date: 3/20/12
 * Time: 8:06 PM
 */

import scala.io._
import cern.colt.matrix.tdouble.impl.{SparseDoubleMatrix2D, SparseDoubleMatrix1D}
import admmutils.ListHelper.list2helper
import cern.colt.matrix.tdouble.{DoubleMatrix1D, DoubleMatrix2D}

object RCV1Data {
  type SampleSet = DoubleMatrix2D
  type OutputSet = DoubleMatrix1D

  def rcv1IDF(nDocs: Int = 23149, nSlices: Int = 1, nFeatures: Int = 47236): List[SampleSet] = {
    val docsPerSlice = nDocs / nSlices
    val sliceGroups = Source.fromFile("etc/data/word_counts.admm.data")
      .getLines
      .take(nDocs)
      .zipWithIndex
      .toList
      .chunk(docsPerSlice)
    var j = 0
      val slices = sliceGroups.map(slice => {
      val data = new SparseDoubleMatrix2D(docsPerSlice, nFeatures)
      j += 1
      for ((line, docIndex) <- slice) {
        for (pair <- line.split(":")) {
          val splits = pair.split(", ")
          val front = splits(0)
          val back = splits(1)
          val feature = front.substring(1, front.length()).toInt
          val weight = back.substring(0, back.length() - 1).toDouble
          data.setQuick(docIndex - (j-1)*docsPerSlice , feature, weight)
        }
      }
      data
    })
    slices
  }

  def labels(index: Int = 0, nDocs: Int = 23149, nSlices: Int = 1): List[OutputSet] = {
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

  def getDataset(nDocs: Int, nFeatures: Int, topicIndex: Int, nSlices: Int): DataSet[SampleSet,OutputSet] = {
    nSlices match {
      case 1 => SingleSet(
      rcv1IDF(nDocs,1,nFeatures).head,
      labels(topicIndex,nDocs,1).head
    )
      case _ => SlicedDataSet(
        rcv1IDF(nDocs,nSlices,nFeatures)
          .zip(labels(topicIndex,nDocs,nSlices))
          .map{case (d,o) => SingleSet(d,o)})
    }
  }

  def getDatasetList(nDocs: Int, nFeatures: Int, topicIndex: Int, nSlices: Int): List[(SampleSet,OutputSet)] = {
    nSlices match {
      case 1 => List( (
        rcv1IDF(nDocs,1,nFeatures).head,
        labels(topicIndex,nDocs,1).head )
      )
      case _ =>
        rcv1IDF(nDocs,nSlices,nFeatures)
          .zip(labels(topicIndex,nDocs,nSlices))
          .map{case (d,o) => (d,o)}
    }
  }


  def main(args: Array[String]) {
    val nSlices = 1000
    val nDocs = 10000
    val nFeatures = 5000
    val topicIndex = 0;
    val data = getDataset(400,200,0,1)
    println(data)
  }


}
