/** ****************************************************************************
  * Disjoint Sets. Specialized implementation for Int elements
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package disjointSet

object WeightedPathCompressedDisjointIntSet {
  def apply(size: Int): WeightedPathCompressedDisjointIntSet =
    new WeightedPathCompressedDisjointIntSet(size)
}


class WeightedPathCompressedDisjointIntSet(override val size: Int)
  extends WeightedPathCompressedDisjointSet[Int]
    with IntElements {

  override final protected def findRoot(i: Int): Int =
    findIndexRoot(i)
}