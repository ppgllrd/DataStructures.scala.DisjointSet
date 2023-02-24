package disjointSet

protected trait IntElements {
  this: DisjointSet[Int] =>

  override protected final def indexOf(x: Int): Int = x

  override protected final def elementOf(i: Int): Int = i
}
