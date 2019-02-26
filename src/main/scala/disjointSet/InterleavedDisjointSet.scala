/** ****************************************************************************
  * Disjoint Sets.
  *
  * Pepe Gallardo, 2019
  *
  * This is Rem's algorithm, RemSP implementation in
  * Experiments on Union-Find Algorithms for the Disjoint-Set
  * Data Structure.
  *   Md. Mostofa Ali Patwary1, Jean Blair2, and Fredrik Manne
  *
  *
  * ****************************************************************************/

package disjointSet

import indexedSet.IndexedSet

trait InterleavedDisjointSet[A] extends DisjointSet[A] {
  protected val parents = Array.tabulate[Int](size)(i => i)
  // number of different components
  protected var nComponents = size

  def numComponents: Int = nComponents

  protected final def indexUnite(i: Int, j: Int, doUnion: Boolean): Boolean = {
    validate(i)
    validate(j)

    var iRoot = i
    var jRoot = j

    var iRootParent = parents(iRoot)
    var jRootParent = parents(jRoot)

    var canBeConnected = true
    while (canBeConnected && (iRootParent != jRootParent)) {
      if (iRootParent < jRootParent) {
        if (iRoot == iRootParent) {
          if (doUnion) {
            parents(iRoot) = jRootParent
            nComponents -= 1
          }
          canBeConnected = false
        } else {
          parents(iRoot) = jRootParent
          iRoot = iRootParent
          iRootParent = parents(iRoot)
        }
      } else {
        if (jRoot == jRootParent) {
          if (doUnion) {
            parents(jRoot) = iRootParent
            nComponents -= 1
          }
          canBeConnected = false
        } else {
          parents(jRoot) = iRootParent
          jRoot = jRootParent
          jRootParent = parents(jRoot)
        }
      }
    }
    canBeConnected
  }

  def areConnected(x: A, y: A): Boolean =
    indexUnite(indexOf(x), indexOf(y), false)

  def union(x: A, y: A): Unit =
    indexUnite(indexOf(x), indexOf(y), true)
}


object InterleavedDisjointSet {
  def fromIndexedSet[A](anIndexedSet: IndexedSet[A]): InterleavedDisjointSet[A] =
    new InterleavedDisjointSet[A] with FromIndexedSet[A] {
      override val indexedSet: IndexedSet[A] = anIndexedSet
    }
}