import disjointSet.{DisjointSet, InterleavedDisjointIntSet, RankedPathCompressedDisjointIntSet, WeightedPathCompressedDisjointIntSet}

object ErdosRenyi extends App {
  def runtimeTest[DSI](makeDisjointSet: Int => DSI)
                      (implicit ev$1: DSI => DisjointSet[Int]) : Double = {
    def experiment(seed: Int, size: Int): Int = {
      val rnd = new scala.util.Random(seed)

      var edges = 0
      val disjointSet = makeDisjointSet(size)
      while (disjointSet.numberOfComponents > 1) {
        val n = rnd.nextInt(size)
        val m = rnd.nextInt(size)
        disjointSet.union(n, m)
        edges += 1
      }
      edges
    }

    val t0 = System.currentTimeMillis()

    val nExperiments = 5000
    for (size <- List.iterate(100, 8)(_ * 2)) {
      var s = 0
      for (seed <- 0 until nExperiments)
        s += experiment(seed, size)
      val avg = s.toDouble / nExperiments
      println(s"$size:\t$avg")
    }

    val t1 = System.currentTimeMillis()
    val runningTime = (t1 - t0) / 1000.0
    println(s"Running time: $runningTime secs.")
    runningTime
  }

  val tInter = runtimeTest(InterleavedDisjointIntSet(_))
  val tWPC = runtimeTest(WeightedPathCompressedDisjointIntSet(_))
  val tRPC = runtimeTest[RankedPathCompressedDisjointIntSet](RankedPathCompressedDisjointIntSet(_))

  println(f"Interleaved is ${100 * (1 - tInter / tWPC)}%.2f%% faster than weighted")
  println(f"Interleaved is ${100 * (1 - tInter / tRPC)}%.2f%% faster than ranked")
  println(f"Ranked is ${100 * (1 - tRPC / tWPC)}%.2f%% faster than weighted")
}
