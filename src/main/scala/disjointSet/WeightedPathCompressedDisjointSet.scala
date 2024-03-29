/** ****************************************************************************
  * Implementation of Disjoint Sets with weighted trees and path compression
  *
  * Pepe Gallardo, 2019
  *
  * See:
  * Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne
  *
  * ****************************************************************************/

package disjointSet

import indexedSet.IndexedSet

/**
 * Implementation of disjoint sets with weighted trees and path compression.
 * See: Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne.
 *
 * @tparam A type of elements in disjoint set.
 * @author Pepe Gallardo
 */
trait WeightedPathCompressedDisjointSet[A] extends DisjointSet[A] {
  // a negative value means element is a root and weight of tree rooted at that node is negation of such number
  protected val parents: Array[Int] = Array.fill[Int](size)(-1)

  // number of different components
  protected var nComponents: Int = size

  def numberOfComponents: Int = nComponents

  final def areConnected(x: A, y: A): Boolean = {
    val (xRoot, xSize) = findRoot(x)
    val (yRoot, ySize) = findRoot(y)
    xRoot == yRoot
  }

  final def union(x: A, y: A): Boolean = {
    val (xRoot, xSize) = findRoot(x)
    val (yRoot, ySize) = findRoot(y)

    if (xRoot == yRoot) {
      false
    } else {
      // link smallest tree below larger one
      // update size for new common root
      if (xSize < ySize) {
        parents(xRoot) = yRoot
        parents(yRoot) -= xSize
      } else {
        parents(yRoot) = xRoot
        parents(xRoot) -= ySize
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
    while (!stop) {
      val rootParent = parents(root)
      if (rootParent < 0) // it's a root
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



object WeightedPathCompressedDisjointSet {
  /**
   * Constructs a disjoint sets with weighted trees and path compression.
   * See: Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne.
   *
   * @tparam A type of elements in disjoint set.
   */
  def fromIndexedSet[A](anIndexedSet: IndexedSet[A]): WeightedPathCompressedDisjointSet[A] =
    new WeightedPathCompressedDisjointSet[A] with FromIndexedSet[A] {
      override val indexedSet: IndexedSet[A] = anIndexedSet
    }
}

