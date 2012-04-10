package data

/**
 * User: jdr
 * Date: 4/9/12
 * Time: 3:07 PM
 */

abstract class DataSet[A,B]
case class SingleSet[A,B](samples: A, output: B) extends DataSet[A,B]
case class SlicedDataSet[A,B](slices: Iterable[SingleSet[A,B]]) extends DataSet[A,B]

