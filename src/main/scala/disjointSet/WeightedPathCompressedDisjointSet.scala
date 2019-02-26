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

trait WeightedPathCompressedDisjointSet[A] extends DisjointSet[A] {
  // number of different components
  protected var nComponents = size

  def numComponents: Int = nComponents

  protected val parents = Array.tabulate[Int](size)(i => i)
  protected val sizes = Array.fill[Int](size)(1)

  protected final def findIndexRoot(i: Int): Int = {
    validate(i)
    var root = i
    var stop = false
    while (!stop) {
      val parent = parents(root)
      if (root == parent)
        stop = true
      else
        root = parent
    }

    // path compression
    var j = i
    while (j != root) {
      val jParent = parents(j)
      parents(j) = root
      j = jParent
    }

    root
  }

  protected def findRoot(x: A): Int =
    findIndexRoot(indexOf(x))

  final def areConnected(x: A, y: A): Boolean =
    findRoot(x) == findRoot(y)

  final def union(x: A, y: A): Unit = {
    val xRoot = findRoot(x)
    val yRoot = findRoot(y)

    if (xRoot != yRoot) {
      val xSize = sizes(xRoot)
      val ySize = sizes(yRoot)

      // link smallest tree below large one
      // update size for new common root
      if (xSize < ySize) {
        parents(xRoot) = yRoot
        sizes(yRoot) += xSize
      } else {
        parents(yRoot) = xRoot
        sizes(xRoot) += ySize
      }
      nComponents -= 1
    }
  }
}


object WeightedPathCompressedDisjointSet {
  def fromIndexedSet[A](anIndexedSet: IndexedSet[A]): WeightedPathCompressedDisjointSet[A] =
    new WeightedPathCompressedDisjointSet[A] with FromIndexedSet[A] {
      override val indexedSet: IndexedSet[A] = anIndexedSet
    }
}

