package utils

/**
 * Created by IntelliJ IDEA.
 * User: Boris
 * Date: 20/03/12
 * Time: 22:36
 * To change this template use File | Settings | File Templates.
 */

class VectorBis(val elements: Array[Double]) extends Serializable {
  def length = elements.length

  def apply(index: Int) = elements(index)

  def + (other: VectorBis): VectorBis = {
    if (length != other.length)
      throw new IllegalArgumentException("Vectors of different length")
    return VectorBis(length, i => this(i) + other(i))
  }

  def - (other: VectorBis): VectorBis = {
    if (length != other.length)
      throw new IllegalArgumentException("Vectors of different length")
    return VectorBis(length, i => this(i) - other(i))
  }

  def dot(other: VectorBis): Double = {
    if (length != other.length)
      throw new IllegalArgumentException("Vectors of different length")
    var ans = 0.0
    var i = 0
    while (i < length) {
      ans += this(i) * other(i)
      i += 1
    }
    return ans
  }

  def * (scale: Double): VectorBis = VectorBis(length, i => this(i) * scale)

  def / (d: Double): VectorBis = this * (1 / d)

  def unary_- = this * -1

  def sum = elements.reduceLeft(_ + _)

  def squaredDist(other: VectorBis): Double = {
    var ans = 0.0
    var i = 0
    while (i < length) {
      ans += (this(i) - other(i)) * (this(i) - other(i))
      i += 1
    }
    return ans
  }

  def dist(other: VectorBis): Double = math.sqrt(squaredDist(other))

  override def toString = elements.mkString("(", ", ", ")")
}

object VectorBis {
  def apply(elements: Array[Double]) = new VectorBis(elements)

  def apply(elements: Double*) = new VectorBis(elements.toArray)

  def apply(length: Int, initializer: Int => Double): VectorBis = {
    val elements = new Array[Double](length)
    for (i <- 0 until length)
      elements(i) = initializer(i)
    return new VectorBis(elements)
  }

  def zeros(length: Int) = new VectorBis(new Array[Double](length))

  def ones(length: Int) = VectorBis(length, _ => 1)

  class Multiplier(num: Double) {
    def * (vec: VectorBis) = vec * num
  }

  implicit def doubleToMultiplier(num: Double) = new Multiplier(num)

  implicit object VectorAccumParam extends spark.AccumulatorParam[VectorBis] {
    def addInPlace(t1: VectorBis, t2: VectorBis) = t1 + t2
    def zero(initialValue: VectorBis) = VectorBis.zeros(initialValue.length)
  }
}
