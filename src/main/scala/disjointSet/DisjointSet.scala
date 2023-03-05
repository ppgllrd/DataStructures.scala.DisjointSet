/** ****************************************************************************
  * Disjoint Sets, aka Union-Find data structure
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package disjointSet

import indexedSet.IndexedSet

trait DisjointSet[A] {
  /**
   * total number of elements in all disjoint components.
   */
  val size: Int

  /**
   * Returns number of disjoint components in disjoint set. 
   * @return number of disjoint components in disjoint set.
   */
  def numberOfComponents: Int

  /**
   * Unites two disjoint components in disjoint set.
   * @param x one element in first component.
   * @param y one element in second component.
   * @return `true` if elements were previously in different disjoint components.
   */
  def union(x: A, y: A): Boolean

  /**
   * Checks whether two elements are in same disjoint component.
   * @param x one element.
   * @param y another element.
   * @return `true` if both elements are in same disjoint set.
   */
  def areConnected(x: A, y: A): Boolean

  // one-to-one correspondence between elements and natural numbers
  protected def indexOf(x: A): Int
  protected def elementOf(i: Int): A

  protected final def validate(i: Int): Unit =
    assert(0 <= i && i < size, s"DisjointSet. element ${elementOf(i)} is not a valid one")
}


object DisjointSet {
  // default implementation uses interleaved implementation
  def fromIndexedSet[A](indexedSet: IndexedSet[A]): DisjointSet[A] =
    InterleavedDisjointSet.fromIndexedSet(indexedSet)
}


object DisjointIntSet {
  // default implementation uses interleaved implementation
  def apply(size: Int): DisjointSet[Int] =
    InterleavedDisjointIntSet(size)
}