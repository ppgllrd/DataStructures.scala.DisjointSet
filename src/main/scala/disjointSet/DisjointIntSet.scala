/** ****************************************************************************
  * Disjoint Sets. Specialized implementation for Int elements
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package disjointSet

object DisjointIntSet {
  def apply(size : Int) : DisjointIntSet =
    new DisjointIntSet(size)
}


class DisjointIntSet(override val size : Int) extends DisjointSet[Int] {
  override protected def elementOf(i: Int): Int = i

  override protected def indexOf(x: Int): Int = x

  override protected def findRoot(i : Int) : Int =
    findIndexRoot(i)
}