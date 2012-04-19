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
import org.apache.hadoop.fs.Path
import spark.{HadoopRDD, SparkContext, RDD}
import org.apache.hadoop.mapred.{TextInputFormat, FileInputFormat, JobConf}
import org.apache.hadoop.io.{Text, LongWritable}
import com.sun.corba.se.spi.protocol.RequestDispatcherDefault
import data.ReutersData._

object ReutersData {

  type IDFId = Int
  type TopicId = Int
  type IDFScore = Double
  type IDFRecord = (IDFId, IDFScore)
  type SampleSet = DoubleMatrix2D
  type OutputSet = DoubleMatrix1D

  trait ReutersRecord {
    def topics: Set[TopicId]
    def idfRecords: Iterable[IDFRecord]
    def containsTopic(topic: TopicId): Boolean = topics.contains(topic)
  }

  case class UntaggedRecord(line: String) extends ReutersRecord {
    val (_topics: Set[TopicId], _idfRecords: Iterable[IDFRecord]) = {
      val splits = line.split("  ")
      val tops: Iterable[TopicId] = splits.head.split(",").map{_.toInt}.toSet
      val recs: Iterable[IDFRecord] = splits.last.split(" ").map(pair =>{
        val splits = pair.split(":")
        (splits.head.toInt, splits.tail.head.toDouble)
      })
      (tops, recs)
    }
    def topics = _topics
    def idfRecords = _idfRecords
  }

  case class TaggedRecord(tag: Int, utRec: UntaggedRecord) extends ReutersRecord {
    val topics = utRec.topics
    val idfRecords = utRec.idfRecords
  }
  object TaggedRecord {
    def parse(line: String) = {
      val splits = line.split(" ", 2)
      TaggedRecord(splits.head.toInt, UntaggedRecord(splits.last))
    }
  }
  object ReutersRecord {
    def parse(line: String): ReutersRecord = {
      TaggedRecord.parse(line)
    }
  }



  val hdfsSitePath = "/usr/local/share/hadoop/conf/hdfs-site.xml"
  val coreSitePath = "/usr/local/share/hadoop/conf/core-site.xml"

  def hdfsJobConf(corePath: String = coreSitePath, hdfsPath: String = hdfsSitePath): JobConf = {
    val jobConf = new JobConf()
    jobConf.addResource(new Path(corePath))
    jobConf.addResource(new Path(hdfsPath))
    jobConf
  }
  def localJobConf = new JobConf()

  def setInputFile(filePath: String, jobConf: JobConf): JobConf =  {
    FileInputFormat.setInputPaths(jobConf, new Path(filePath))
    jobConf
  }

  class TextFileRDD(sc: SparkContext,
                    filePath: String,
                    jobConf: JobConf,
                    maxSplits: Int = 10)
    extends HadoopRDD(sc,
      setInputFile(filePath,jobConf),
      new TextInputFormat().getClass,
      new LongWritable().getClass.asInstanceOf[Class[LongWritable]],
      new Text().getClass.asInstanceOf[Class[Text]],
      maxSplits)

  object TextFileRDD {
    def hdfsTextRDD(sc: SparkContext, filePath: String) = {
      new TextFileRDD(sc, filePath, hdfsJobConf())
    }
    def localTextRDD(sc: SparkContext, filePath: String) = {
      new TextFileRDD(sc, filePath, localJobConf)
    }
  }




  trait ReutersSet {
    def samples: SampleSet
    def outputs(topicId: TopicId): OutputSet
    def generateReutersSet(topicId: TopicId) = (samples, outputs(topicId))
  }

  class UntaggedReutersSet(records: scala.Seq[UntaggedRecord], n: Int) extends ReutersSet {
    val m = records.size
    def samples: SampleSet = {
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
    def outputs(topicId: TopicId): OutputSet = {
      val vector = DoubleFactory1D.sparse.make(m)
      records.zipWithIndex.foreach{
        case (record, recordInd) => {
          if (record.topics.contains(topicId)) vector.setQuick(recordInd,1.0)
        }}
      vector
    }
  }

  class ReutersRDD(sc: SparkContext, filePath: String, jobConf: JobConf, maxSplits: Int = 10 ) extends TextFileRDD(sc, filePath, jobConf, maxSplits) {
    val records = this.map(record => TaggedRecord.parse(record._2.toString()))
    val n = 1000
    def splitSets(nSplits: Int): RDD[ReutersSet] = {
      class ReutersRDDSet(group: (Int, scala.Seq[TaggedRecord])) extends ReutersSet {
        val records = group._2.map(rec => rec.utRec)
        val m = records.size
        def samples: SampleSet = {
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
        def outputs(topicId: TopicId): OutputSet = {
          val vector = DoubleFactory1D.sparse.make(m)
          records.zipWithIndex.foreach{
            case (record, recordInd) => {
              if (record.topics.contains(topicId)) vector.setQuick(recordInd,1.0)
            }}
          vector
        }
      }
      records.groupBy{case TaggedRecord(tag, record) => tag % nSplits}
        .map{group => new ReutersRDDSet(group)}
    }
  }

  object ReutersRDD {
    def hdfsTextRDD(sc: SparkContext, filePath: String) = {
      new ReutersRDD(sc, filePath, hdfsJobConf())
    }
    def localTextRDD(sc: SparkContext, filePath: String) = {
      new ReutersRDD(sc, filePath, localJobConf)
    }
  }



  def getBalancedSet(nDocs: Int, nFeatures: Int, topicId: TopicId, nSlices: Int, proportion: Double) = {
    val fn = "etc/data/rcv1_topics_train.svm"
    val chunkSize = nDocs/nSlices
    val records = Source.fromFile(fn).getLines().map{new UntaggedRecord(_)}.toIterable
    val nPos = math.floor(nDocs*proportion).toInt
    val posRecords = records.filter(record => record.containsTopic(topicId)).take(nPos).toList
    val actualNPos = posRecords.length
    val actualNNeg = nDocs - actualNPos
    val negRecords = records.filter(record => !record.containsTopic(topicId)).take(actualNNeg).toList
    val allRecords = posRecords++negRecords
    val combined = scala.util.Random.shuffle(allRecords).toList
    val slices = combined.chunk(chunkSize).map(slice =>{
      val data = new UntaggedReutersSet(slice, nFeatures).generateReutersSet(topicId)
      SingleSet(data._1, data._2)
    })
    SlicedDataSet(slices)
  }



  def main(args: Array[String]) {
    val sc = new SparkContext("local", "test")
    val reutersRDD = ReutersRDD.hdfsTextRDD(sc, "/user/hduser/data")
    //val localReuters = ReutersRDD.localTextRDD(sc, "etc/data/labeled_rcv1.admm.data")
    reutersRDD.splitSets(100).foreach(set => println((1 to 100).map{set.outputs(_).cardinality()}))
    //localReuters.splitSets(200).foreach(println)
  }

}
