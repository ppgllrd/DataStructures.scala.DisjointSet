/** ****************************************************************************
  * Disjoint Sets. Specialized implementation for Int elements
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package disjointSet

object InterleavedDisjointIntSet {
  def apply(size : Int) : InterleavedDisjointIntSet =
    new InterleavedDisjointIntSet(size)
}


class InterleavedDisjointIntSet(override val size: Int)
  extends InterleavedDisjointSet[Int]
    with IntElements {

  override final def areConnected(i: Int, j: Int): Boolean =
    indexUnite(i, j, false)

  override final def union(i: Int, j: Int): Unit =
    indexUnite(i, j, true)
}


