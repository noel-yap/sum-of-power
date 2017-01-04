package com.github.noel.yap

import scala.annotation.tailrec

object Primes {
  private lazy val notDivisibleBy2: Stream[Long] = 3L #:: notDivisibleBy2.map(_ + 2)
  private lazy val notDivisibleBy2Or3: Stream[Long] = notDivisibleBy2
    .grouped(3)
    .map(_.slice(1, 3))
    .flatten
    .toStream
  private lazy val notDivisibleBy2Or3Or5: Stream[Long] = notDivisibleBy2Or3
    .grouped(10)
    .map { g =>
      g.slice(1, 7) ++ g.slice(8, 10)
    }
    .flatten
    .toStream

  lazy val primes: Stream[Long] = 2L #::
    notDivisibleBy2.head #::
    notDivisibleBy2Or3.head #::
    notDivisibleBy2Or3Or5.filter { i =>
      i < 49 || primes.takeWhile(_ <= Math.sqrt(i).toLong).forall(i % _ != 0)
    }

  def <(n: Long): Stream[Long] = primes.takeWhile(_ <= n)

  def largestPower(n: Long, p: Long): Int = {
    @tailrec
    def _largestPower(x: Int, n: Long): Int = {
      if (n % p != 0) {
        x
      } else {
        _largestPower(x + 1, n / p)
      }
    }

    _largestPower(0, n)
  }

  def factorize(n: Long): Map[Long, Int] = {
    val primeFactorsOfN = (Primes < n).filter(n % _ == 0)

    primeFactorsOfN.map { p =>
      (p, largestPower(n, p))
    }.toMap
  }
}
