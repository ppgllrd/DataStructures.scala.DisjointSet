/** ****************************************************************************
  * Disjoint Sets
  *
  * Pepe Gallardo, 2019
  *
  * See:
  *   Algorithms, 4th Edition by Robert Sedgewick and Kevin Wayne
  *
  * ****************************************************************************/

package disjointSet

import indexedSet.IndexedSet

trait DisjointSet[A] {
  // size is total number of elements in all sets
  protected val size : Int

  protected def indexOf(x : A) : Int
  protected def elementOf(i : Int) : A

  // number of different components
  protected var nComponents = size
  def numComponents: Int = nComponents

  protected val roots = Array.tabulate[Int](size)(i => i)
  protected val sizes = Array.fill[Int](size)(1)

  protected def validate(i : Int): Unit =
    assert(0 <= i && i < size, s"DisjointSet. element ${elementOf(i)} is not a valid one")

  protected def findIndexRoot(i : Int) : Int = {
    validate(i)
    var root = i
    var stop = false
    while(!stop) {
      val parent = roots(root)
      if(root == parent)
        stop = true
      else
        root = parent
    }

    // path compression
    var p = i
    while (p != root) {
      val pRoot = roots(p)
      roots(p) = root
      p = pRoot
    }

    root
  }

  protected def findRoot(x : A) : Int =
    findIndexRoot(indexOf(x))

  def areConnected(x: A, y: A): Boolean =
    findRoot(x) == findRoot(y)

  def union(x: A, y: A): Unit = {
    val xRoot = findRoot(x)
    val yRoot = findRoot(y)

    if (xRoot != yRoot) {
      if (sizes(xRoot) < sizes(yRoot)) {
        roots(xRoot) = yRoot
        sizes(xRoot) += sizes(yRoot)
      } else {
        roots(yRoot) = xRoot
        sizes(yRoot) += sizes(xRoot)
      }
      nComponents -= 1
    }
  }
}


object DisjointSet {
  def fromIndexedSet[A](indexedSet : IndexedSet[A]) : DisjointSet[A] =
    new DisjointSet[A] {
      override protected val size: Int = indexedSet.size

      override protected def indexOf(x: A): Int = indexedSet.indexOf(x)

      override protected def elementOf(i: Int): A = indexedSet.elementOf(i)
    }
}