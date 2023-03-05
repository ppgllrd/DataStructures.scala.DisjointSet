import disjointSet.{DisjointSet, InterleavedDisjointIntSet, RankedPathCompressedDisjointIntSet, WeightedPathCompressedDisjointIntSet}

import scala.collection._

object Correctness extends App {
  val n = 1002

  def test1(mkDS: () => DisjointSet[Int]): Unit = {
    val ds = mkDS()
    for (i <- 0 until n by 3)
      ds.union(i, i + 1)

    for (i <- 0 until n by 3) {
      assert(ds.areConnected(i, i + 1))
      assert(!ds.areConnected(i, i + 2))
      assert(!ds.areConnected(i + 1, i + 2))
    }

    for (i <- 0 until n by 3)
      ds.union(i, i + 2)

    for (i <- 0 until n by 3) {
      assert(ds.areConnected(i, i + 1))
      assert(ds.areConnected(i, i + 2))
      assert(ds.areConnected(i + 1, i + 2))

      if (i + 3 < n)
        assert(!ds.areConnected(i, i + 3))
    }
  }

  test1(() => InterleavedDisjointIntSet(n))
  test1(() => WeightedPathCompressedDisjointIntSet(n))
  test1(() => RankedPathCompressedDisjointIntSet(n))

  def test2(mkDS: () => DisjointSet[Int]): Unit = {
    val ds = mkDS()
    val rnd = new scala.util.Random()
    val set = mutable.Set[(Int, Int)]()
    for (i <- 0 until n / 2) {
      val x = rnd.nextInt(n)
      val y = rnd.nextInt(n)
      set += ((x, y))
      ds.union(x, y)
    }

    for ((x, y) <- set)
      assert(ds.areConnected(x, y))

    for ((x, y) <- set)
      assert(ds.areConnected(y, x))

    for ((x, y) <- set)
      for ((_, z) <- set.filter(_._1 == y))
        assert(ds.areConnected(x, z))

    for ((x, y) <- set)
      for ((_, z) <- set.filter(_._1 == y))
        for ((_, v) <- set.filter(_._1 == z))
          assert(ds.areConnected(x, v))

    for ((x, y) <- set)
      for ((_, z) <- set.filter(_._1 == y))
        for ((_, v) <- set.filter(_._1 == z))
          for ((_, u) <- set.filter(_._1 == v))
            assert(ds.areConnected(x, u))
  }

  test2(() => InterleavedDisjointIntSet(n))
  test2(() => WeightedPathCompressedDisjointIntSet(n))
  test2(() => RankedPathCompressedDisjointIntSet(n))

  def test3(mkDS: () => DisjointSet[Int]): Unit = {
    val ds = mkDS()
    val rnd = new scala.util.Random()
    val set = mutable.Set[(Int, Int)]()
    for (i <- 0 until n / 2) {
      val x = rnd.nextInt(n)
      val y = rnd.nextInt(n)
      if (x != y) {
        set += ((x, y))
        ds.union(x, y)
      }
    }

    val close = closure(set)
    assert(close.size == ds.numberOfComponents)

    for (s <- close)
      for (List(x, y) <- s.toList.sliding(2))
        assert(ds.areConnected(x, y))
    for (s <- close)
      for (List(x, y, z) <- s.toList.sliding(3)) {
        assert(ds.areConnected(x, y))
        assert(ds.areConnected(x, z))
        assert(ds.areConnected(y, z))
      }

    for (List(s1, s2) <- close.toList.sliding(2)) {
      for (x <- s1)
        for (y <- s2)
          assert(!ds.areConnected(x, y))
    }

    for (list <- close.toList.sliding(3)) {
      list match {
        case List(s1, s2, s3) =>
          for (x <- s1)
            for (y <- s2)
              for (z <- s3) {
                assert(!ds.areConnected(x, y))
                assert(!ds.areConnected(x, z))
                assert(!ds.areConnected(y, z))
              }
        case List(s1, s2) =>
          for (x <- s1)
            for (y <- s2)
              assert(!ds.areConnected(x, y))
      }
    }

  }

  def closure(set: mutable.Set[(Int, Int)]): mutable.Set[mutable.Set[Int]] = {
    var classes = mutable.Set[mutable.Set[Int]]()

    for (x <- 0 until n) {
      classes += mutable.Set(x)
    }

    for ((x, y) <- set) {
      classes += mutable.Set(x, y)
    }

    var stop = false
    while (!stop) {
      stop = true

      val classes2 = mutable.Set[mutable.Set[Int]]()
      for (set <- classes) {
        val clon = mutable.Set[Int]()
        for (x <- set)
          clon += x
        classes2 += clon
      }

      for (c1 <- classes) {
        for (c2 <- classes) {
          if (c1 != c2 && c1.intersect(c2).nonEmpty && classes2.contains(c1) && classes2.contains(c2)) {
            classes2.remove(c1)
            classes2.remove(c2)
            classes2.add(c1 union c2)
            stop = false
          }
        }
      }
      classes = classes2
    }
    classes
  }

  test3(() => InterleavedDisjointIntSet(n))
  test3(() => WeightedPathCompressedDisjointIntSet(n))
  test3(() => RankedPathCompressedDisjointIntSet(n))
}

