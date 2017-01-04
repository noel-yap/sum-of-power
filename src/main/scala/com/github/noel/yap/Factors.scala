package com.github.noel.yap

object Factors {
  private def dotPower(set: Iterable[(Long, Int)]): Long = {
    set.map { case (p, x) =>
      Math.pow(p, x).toLong
    }.product
  }

  private def lowerFactors(primeFactors: Map[Long, Int]): Set[Long] = {
    val primeFactorsSpanPoint = primeFactors.filter { e =>
      e._2 == primeFactors.values.max
    }.head._1

    factors(primeFactors + (primeFactorsSpanPoint -> (primeFactors(primeFactorsSpanPoint) + 1) / 2))
        .filter(_ <= Math.sqrt(dotPower(primeFactors)))
  }

  private def factors(primeFactors: Map[Long, Int]): Set[Long] = {
    def cross(lhs: List[Int], rhs: List[List[Int]]): List[List[Int]] = {
      for {l <- lhs; r <- rhs} yield l :: r
    }

    primeFactors
        .map(x => Range(0, x._2 + 1).toList)
        .foldRight(List(List[Int]()))(cross)
        .map { x: List[Int] =>
          dotPower(primeFactors.keys.zip(x))
        }.toSet
  }

  def factorPairs(n: Long): Set[(Long, Long)] = {
    if (n < 1) {
      factorPairs(-n)
          .map { case (lhs, rhs) =>
            (-lhs, rhs)
          }
    } else if (n == 1) {
      Set((1, 1), (-1, -1))
    } else {
      val primeFactorsOfN = Primes.factorize(n)

      val lf = lowerFactors(primeFactorsOfN)
          .map { f =>
            (f, n / f)
          }

      val positiveFactors = lf ++ lf.filter(p => p._1 != p._2).map(_.swap)
      positiveFactors ++ positiveFactors.map { case (lhs, rhs) =>
        (-lhs, -rhs)
      }
    }
  }
}
