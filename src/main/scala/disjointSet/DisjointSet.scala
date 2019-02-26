/** ****************************************************************************
  * Disjoint Sets, aka Union-Find data structure
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package disjointSet

import indexedSet.IndexedSet

trait DisjointSet[A] {
  // total number of elements in all sets
  val size: Int

  // number of different components
  def numComponents: Int

  def union(x: A, y: A): Unit

  def areConnected(x: A, y: A): Boolean

  // one-to-one correspondence between elements and natural numbers
  protected def indexOf(x: A): Int
  protected def elementOf(i: Int): A

  protected final def validate(i: Int): Unit =
    assert(0 <= i && i < size, s"DisjointSet. element ${elementOf(i)} is not a valid one")
}


object DisjointSet {
  // default implementation uses interleaved implementation
  def fromIndexedSet[A](indexedSet: IndexedSet[A]): InterleavedDisjointSet[A] =
    InterleavedDisjointSet.fromIndexedSet(indexedSet)
}


object DisjointIntSet {
  // default implementation uses interleaved implementation
  def apply(size: Int): InterleavedDisjointIntSet =
    InterleavedDisjointIntSet(size)
}