package com.github.noel.yap

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PolynomialSpec extends FlatSpec with Matchers {
  "Factors of n^3" should "be n^3" in {
    Polynomial(0, 0, 0, 1).factor should contain only (Polynomial(0, 0, 0, 1) -> 1)
  }

  "Factors of n^3 + 3n^2 + 3n + 1" should "be (n + 1)^3" in {
    Polynomial(1, 3, 3, 1).factor should contain only (Polynomial(1, 1) -> 3)
  }

  "Factors of 10n^2 + 31n + 24" should "be (2n + 3)(5n + 8)" in {
    Polynomial(24, 31, 10).factor should contain only (Polynomial(2, 3) -> 1, Polynomial(5, 8) -> 1)
  }

  "Trying to factor an unfactorable polynomial" should "not crash" in {
    Polynomial(1, 2, 3, 4).factor should contain only (Polynomial(1, 2, 3, 4) -> 1)
  }

  "toString" should "be -n^2 - n - 1" in {
    Polynomial(-1, -1, -1).toString should be ("-n^2 - n - 1")
  }

  "toString" should "be -2n^2 - 2n - 2" in {
    Polynomial(-2, -2, -2).toString should be ("-2n^2 - 2n - 2")
  }

  "toString" should "be -2n" in {
    Polynomial(0, -2).toString should be ("-2n")
  }
  //println(Polynomial(1, 3, 3, 1))
  //println(Polynomial(0, 0, 1, 3, 3, 1))
}
