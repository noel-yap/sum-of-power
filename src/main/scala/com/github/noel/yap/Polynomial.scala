package com.github.noel.yap

import scala.annotation.tailrec
import scala.language.postfixOps

case class Polynomial(coefficients: Long*) {
  override def toString: String = {
    def _toString(coefficients: Seq[Long]): String = {
      def toTermString(coefficient: Long, exponent: Int): String = {
        val coefficientPart = if (coefficient.abs != 1 || exponent == 0) {
          coefficient.toString
        } else if (coefficient == 1) {
          ""
        } else {
          "-"
        }
        val variablePart = if (exponent == 0) {
          ""
        } else if (exponent == 1) {
          "n"
        } else {
          s"n^${exponent}"
        }

        s"${coefficientPart}${variablePart}"
      }

      val coefficientExponentPairs = coefficients
          .zipWithIndex
          .reverse
          .filter(_._1 != 0)
      val head = coefficientExponentPairs.head
      val tail = coefficientExponentPairs.tail

      val firstTerm = toTermString(head._1, head._2)
      tail
          .foldLeft(firstTerm) { case (lhs, rhs) =>
            val (coefficient, exponent) = rhs
            val operator = if (coefficient < 0) {
              "-"
            } else {
              "+"
            }

            s"${lhs} ${operator} ${toTermString(coefficient.abs, exponent)}"
          }
    }

    val factors = this.factor
    if (factors.size == 1 && factors.head._2 == 1) {
      _toString(coefficients)
    } else {
      factors.toList.sortWith { (lhs, rhs) =>
        lhs._1.order < rhs._1.order ||
            lhs._1.order == rhs._1.order && lhs._2 > rhs._2 ||
            lhs._1.order == rhs._1.order && lhs._2 == rhs._2 && lhs._1.coefficients.zip(rhs._1.coefficients)
                .dropWhile(e => e._1 == e._2)
                .take(1)
                .map(e => e._1 < e._2)
                .head
      }.map { case (polynomial, exponent) =>
        val p = if (polynomial.numberOfTerms == 1) {
          _toString(polynomial.coefficients)
        } else {
          s"(${polynomial})"
        }

        val e = if (exponent == 1) {
          ""
        } else {
          s"^${exponent}"
        }

        s"${p}${e}"
      }
    }.mkString(" × ")
  }

  def apply(n: Int): BigInt = {
    coefficients
        .zipWithIndex
        .map { case (coefficient, i) =>
          coefficient * BigInt(n).pow(i)
        }.sum
  }

  def ==(that: Polynomial): Boolean = this.coefficients == that.coefficients

  def -(that: Polynomial): Polynomial = {
    val length = Math.max(coefficients.length, that.coefficients.length)
    val lhs = coefficients.padTo(length, 0L)
    val rhs = that.coefficients.padTo(length, 0L)

    Polynomial(lhs.zip(rhs).map { case (l, r) =>
      l - r
    }: _*)
  }

  def /(n: Long): Polynomial = {
    Polynomial(coefficients.map(_ / n): _*)
  }

  def order(): Int = {
    coefficients.length - 1
  }

  def numberOfTerms(): Int = {
    coefficients.count(_ != 0)
  }

  def factor: Map[Polynomial, Int] = {
    // TODO: @tailrec
    def _factor(coefficients: Seq[Long]): Seq[Polynomial] = {
      if (coefficients.head == 0) {
        Polynomial(0, 1) +: _factor(coefficients.tail)
      } else if (coefficients.length < 3) {
        Seq(Polynomial(coefficients: _*))
      } else {
        val highestPowerCoefficientFactorPairs = Factors.factorPairs(coefficients.last).toList.sorted.reverse
        val lowestPowerCoefficientFactorPairs = Factors.factorPairs(coefficients.head).toList.sorted

        val factorPairsCartesianProduct = (CartesianProduct(highestPowerCoefficientFactorPairs) * lowestPowerCoefficientFactorPairs)
            .product

        factorPairsCartesianProduct
            .flatMap { case List(hpcfp, lpcfp) =>
              val lhsCoefficients = (hpcfp.asInstanceOf[(Long, Long)]._2, lpcfp.asInstanceOf[(Long, Long)]._2)
              val rhsCoefficients = this.rhsCoefficients(
                coefficients,
                lhsCoefficients,
                (hpcfp.asInstanceOf[(Long, Long)]._1, lpcfp.asInstanceOf[(Long, Long)]._1))

              if (rhsCoefficients == None) {
                None
              } else {
                Some((lhsCoefficients, rhsCoefficients))
              }
            }.map { case (lhsCoefficients, rhsCoefficients) =>
          Polynomial(List(lhsCoefficients._1, lhsCoefficients._2): _*) +: _factor(rhsCoefficients.get)
        }.headOption.getOrElse(Seq(Polynomial(coefficients :_*)))
      }
    }

    val f = _factor(coefficients)
        .groupBy(p => p)
        .mapValues(_.length)
        .map { case (p, e) =>
          if (p == Polynomial(0L, 1L)) {
            (Polynomial(List.fill(e)(0L) :+ 1L: _*), 1)
          } else {
            (p, e)
          }
        }

    if (f.size == 1) {
      f
    } else {
      f.filter { case (p, e) =>
        p != Polynomial(1)
      }
    }
  }

  private def rhsCoefficients(
      coefficients: Seq[Long],
      lhsHeadLastCoefficients: (Long, Long),
      rhsHeadLastCoefficients: (Long, Long)): Option[Seq[Long]] = {
    @tailrec
    def _rhsCoefficients(
        accum: Seq[Long],
        reverseProductCoefficients: Seq[Long]): Option[Seq[Long]] = {
      if (reverseProductCoefficients.length == 2) {
        Some(accum)
      } else {
        val numerator = reverseProductCoefficients.head - accum.head * lhsHeadLastCoefficients._1
        val denominator = lhsHeadLastCoefficients._2

        if (numerator % denominator != 0) {
          None
        } else {
          _rhsCoefficients(numerator / denominator +: accum, reverseProductCoefficients.tail)
        }
      }
    }

    _rhsCoefficients(Seq(rhsHeadLastCoefficients._2), coefficients.reverse.tail)
        .map { rhsC =>
          rhsHeadLastCoefficients._1 +: rhsC
        }.filter { rhsC =>
      coefficients.tail.head == rhsC.head * lhsHeadLastCoefficients._2.toLong + rhsC.tail.head * lhsHeadLastCoefficients._1.toLong
    }
  }
}
