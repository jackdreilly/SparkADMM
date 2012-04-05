package admmutils

/**
 * User: jdr
 * Date: 3/27/12
 * Time: 6:31 PM
 */
class ListHelper[T](ls: List[T]) {
  /**@param size  The size of each sub list */
  def chunk(size: Int) = List.range(0, ls.size, size).map {
    i => ls.slice(i, i + size)
  }
}

object ListHelper {
  implicit def list2helper[T](ls: List[T]) = new ListHelper(ls)
}


class IteratorHelper[T](ls: Iterator[T]) {
  /**@param size  The size of each sub list */
  def chunk(size: Int) = Iterator.range(0, ls.size, size).map {
    i => ls.slice(i, i + size)
  }
}

object IteratorHelper {
  implicit def iter2helper[T](ls: Iterator[T]) = new IteratorHelper(ls)
}


