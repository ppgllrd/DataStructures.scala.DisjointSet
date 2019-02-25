import disjointSet.DisjointIntSet

object ErdosRenyi extends App {

  def experiment(seed : Int, size : Int) : Int = {
    val rnd = new scala.util.Random(seed)

    var edges = 0
    val disjointSet = DisjointIntSet(size)
    while (disjointSet.numComponents > 1) {
      val n = rnd.nextInt(size)
      val m = rnd.nextInt(size)
      disjointSet.union(n, m)
      edges += 1
    }
    edges
  }

  val nExperiments = 10000
  for(size <- List.iterate(100,8)(_*2)) {
    var s = 0
    for(seed <-  0 until nExperiments)
      s += experiment(seed, size)
    val avg = s.toDouble / nExperiments
    println(s"$size:\t$avg")
  }
}
