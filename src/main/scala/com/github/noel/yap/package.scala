package com.github.noel

package object yap {
  implicit class _RichInt(n: Int) {
    private def fact(n: Long): BigInt = {
      lazy val f: Memo.==>[(BigInt, Long), BigInt] = Memo {
        case (a, 0) => a
        case (a, 1) => a
        case (a, n) => f(a * n, n - 1)
      }

      f(BigInt(1), n)
    }

    def ! : BigInt = fact(n)
    def P(k: Long): BigInt = fact(n) / fact(n - k)
    def C(k: Long): Long = (P(k) / fact(k)).toLong
  }

  def gcd(lhs: Long, rhs: Long): Long = {
    if (rhs == 0) {
      lhs.abs
    } else {
      gcd(rhs, lhs % rhs)
    }
  }

  def lcm(lhs: Long, rhs: Long): Long = {
    (lhs * rhs).abs / gcd(lhs, rhs)
  }

  implicit class _RichLongWithPolynomial(n: Long) {
    def *(polynomial: Polynomial): Polynomial = {
      Polynomial(polynomial.coefficients.map(n * _) :_*)
    }
  }

  implicit class _RichLongWithΣp(n: Long) {
    def *(σp: Σp): Σp = {
      val g = gcd(n, σp.denominator)

      Σp(n / g * σp.numerator, σp.denominator / g)
    }
  }
}
