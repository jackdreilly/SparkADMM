package data

/**
 * User: jdr
 * Date: 3/20/12
 * Time: 8:06 PM
 */

import scala.io._
import cern.colt.matrix.tdouble.impl.{SparseDoubleMatrix2D, SparseDoubleMatrix1D}
import admmutils.ListHelper.list2helper
import cern.colt.matrix.tdouble.{DoubleFactory1D, DoubleFactory2D, DoubleMatrix1D, DoubleMatrix2D}
import data.ReutersData.{ReutersSet, ReutersRecord}

object ReutersData {
  val fn = "etc/data/rcv1_topics_train.svm"
  type IDFId = Int
  type TopicId = Int
  type IDFScore = Double
  type IDFRecord = (IDFId, IDFScore)
  type SampleSet = DoubleMatrix2D
  type OutputSet = DoubleMatrix1D
  class ReutersRecord(line: String) {
    val (topics: Set[TopicId], idfRecords: Iterable[IDFRecord]) = {
      val splits = line.split("  ")
      val tops: Iterable[TopicId] = splits.head.split(",").map{_.toInt}.toSet
      val recs: Iterable[IDFRecord] = splits.last.split(" ").map(pair =>{
        val splits = pair.split(":")
        (splits.head.toInt, splits.tail.head.toDouble)
      })
      (tops, recs)
    }
    def containsTopic(topicId: TopicId) = topics.contains(topicId)
  }
  class ReutersSet(records: Iterable[ReutersRecord], n: Int) {
    val m = records.size
    def generateSamples: SampleSet = {
      val matrix = DoubleFactory2D.sparse.make(m,n)
      records.zipWithIndex.foreach{
        case (record, recordInd) => {
          record.idfRecords.filter{case (idfId, idfScore) => idfId < n}.foreach{
            case (idfId, idfScore) => {
              matrix.setQuick(recordInd, idfId,idfScore)
            }
          }
        }
      }
      matrix
    }
    def generateOutputs(topicId: TopicId): OutputSet = {
      val vector = DoubleFactory1D.sparse.make(m)
      records.zipWithIndex.foreach{
        case (record, recordInd) => {
          if (record.topics.contains(topicId)) vector.setQuick(recordInd,1.0)
      }}
      vector
    }
    def generateReutersSet(topicId: TopicId) = (generateSamples, generateOutputs(topicId))
  }
  def getBalancedSet(nDocs: Int, nFeatures: Int, topicId: TopicId, nSlices: Int, proportion: Double) = {
    val chunkSize = nDocs/nSlices
    val records = Source.fromFile(fn).getLines().map{new ReutersRecord(_)}.toIterable
    val nPos = math.floor(nDocs*proportion).toInt
    val posRecords = records.filter{_.containsTopic(topicId)}.take(nPos).toList
    val actualNPos = posRecords.length
    val actualNNeg = nDocs - actualNPos
    val negRecords = records.filter{!_.containsTopic(topicId)}.take(actualNNeg).toList
    val allRecords = posRecords++negRecords
    val combined = scala.util.Random.shuffle(allRecords).toList
    val slices = combined.chunk(chunkSize).map(slice =>{
      val data = new ReutersSet(slice, nFeatures).generateReutersSet(topicId)
      SingleSet(data._1, data._2)
    })
    SlicedDataSet(slices)
  }

}
