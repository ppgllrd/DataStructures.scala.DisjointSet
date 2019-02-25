package indexedSet

trait IndexedSet[A] {
  // total number of elements in set
  def size : Int

  // Should return a different i (0 <= i < size) for each x in set
  def indexOf(x : A) : Int

  // elementOf(indexOf(x)) == x
  def elementOf(i : Int) : A
}
