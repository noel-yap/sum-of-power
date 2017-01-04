package com.github.noel.yap

class Σp(val numerator: Polynomial, val denominator: Long) {
  override def toString: String = {
    s"(${numerator}) / ${denominator}"
  }

  def -(rhs: Σp): Σp = {
    val d = lcm(denominator, rhs.denominator)

    Σp(
      d / denominator * numerator - d / rhs.denominator * rhs.numerator,
      d)
  }

  def /(rhs: Long): Σp = {
    Σp(numerator, rhs * denominator)
  }

  def apply(n: Int): BigInt = {
    numerator(n) / denominator
  }
}

object Σp {
  def apply(power: Int): Σp = {
    lazy val f: Memo.==>[Int, Σp] = Memo {
      case 0 => Σp(Polynomial(0L, 1L), 1L)
      case p => {
        val init = new Σp(
          Polynomial(0L +: (1 to p + 1).map { i =>
            p + 1 C i
          } :_*),
          1)

        (0 until p).map { i =>
          (p + 1 C i) * f(i)
        }.foldLeft(init) { (lhs, rhs) =>
          lhs - rhs
        } / (p + 1)
      }
    }

    f(power)
  }

  def apply(numerator: Polynomial, denominator: Long): Σp = {
    val g = numerator.coefficients.foldLeft(denominator) { (lhs, rhs) =>
      gcd(lhs, rhs)
    }

    new Σp(numerator / g, denominator / g)
  }
}
