package indexedSet

object IndexedIntSet {
  def apply(size : Int) : IndexedIntSet =
    new IndexedIntSet(size)
}


class IndexedIntSet(val size : Int) extends IndexedSet[Int] {
  override def indexOf(x: Int): Int = x

  override def elementOf(i: Int): Int = i
}
