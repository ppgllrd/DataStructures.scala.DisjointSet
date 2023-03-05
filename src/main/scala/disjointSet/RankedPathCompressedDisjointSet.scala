/** ****************************************************************************
 * Disjoint Sets. Specialized implementation for Int elements
 *
 * Pepe Gallardo, 2023
 *
 * ****************************************************************************/

package disjointSet

import indexedSet.IndexedSet

/**
 * Implementation of disjoint sets with ranked trees and path compression.
 *
 * @tparam A type of elements in disjoint set.
 * @author Pepe Gallardo
 */
trait RankedPathCompressedDisjointSet[A] extends DisjointSet[A] {
  // a negative value means element is a root and rank of tree rooted at that node is negation of such number minus 1
  protected val parents: Array[Int] = Array.fill[Int](size)(-1)

  // number of different components
  protected var nComponents: Int = size

  def numberOfComponents: Int = nComponents

  final def areConnected(x: A, y: A): Boolean = {
    val (xRoot, xRank) = findRoot(x)
    val (yRoot, yRank) = findRoot(y)
    xRoot == yRoot
  }

  final def union(x: A, y: A): Boolean = {
    val (xRoot, xRank) = findRoot(x)
    val (yRoot, yRank) = findRoot(y)

    if (xRoot == yRoot) {
      false
    } else {
      // link tree with smallest rank below the other
      if (xRank < yRank) {
        parents(xRoot) = yRoot
      } else if (xRank > yRank) {
        parents(yRoot) = xRoot
      } else {
        parents(yRoot) = xRoot
        parents(xRoot) -= 1 // same ranks, new parent gets its rank increased
      }
      nComponents -= 1
      true
    }
  }

  protected def findRoot(x: A): (Int, Int) =
    findIndexRoot(indexOf(x))

  protected final def findIndexRoot(i: Int): (Int, Int) = {
    validate(i)
    var root = i
    var stop = false
    while(!stop) {
      val rootParent = parents(root)
      if(rootParent < 0) // it's a root
        stop = true
      else
        root = rootParent
    }

    // path compression
    var node = i
    while (node != root) {
      val nodeParent = parents(node)
      parents(node) = root
      node = nodeParent
    }

    val weight = -parents(root)
    (root, weight)
  }
}


object RankedPathCompressedDisjointSet {
  /** Constructs a disjoint set with ranked trees and path compression.
   *
   * @param anIndexedSet the indexed set with elements for constructing new disjoint set.
   * @tparam A type of elements in disjoint set.
   * @return a new disjoint set for provided elements.
   */
  def fromIndexedSet[A](anIndexedSet: IndexedSet[A]): RankedPathCompressedDisjointSet[A] =
    new RankedPathCompressedDisjointSet[A] with FromIndexedSet[A] {
      override val indexedSet: IndexedSet[A] = anIndexedSet
    }
}

