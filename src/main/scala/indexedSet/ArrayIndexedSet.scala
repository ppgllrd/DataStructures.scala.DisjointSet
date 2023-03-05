/** ****************************************************************************
  * An implementation of an indexed set of elements
  * by using an Array and an Map
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package indexedSet

object ArrayIndexedSet {
  def apply[A](elements: Array[A]): ArrayIndexedSet[A] =
    new ArrayIndexedSet(elements)
}


class ArrayIndexedSet[A](elements: Array[A]) extends IndexedSet[A] {
  override val size: Int = elements.length

  private val indexes = {
    val map = scala.collection.mutable.Map[A, Int]()
    for (i <- elements.indices)
      map(elements(i)) = i
    map
  }

  override def indexOf(x: A): Int = indexes(x)

  override def elementOf(i: Int): A = elements(i)
}
