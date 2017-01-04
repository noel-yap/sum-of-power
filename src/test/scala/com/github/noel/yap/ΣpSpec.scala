package com.github.noel.yap

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ΣpSpec extends FlatSpec with Matchers {
  "Σp(4)(0)" should "be 0" in {
    Σp(4)(0) should be (0)
  }

  "Σp(4)(1)" should "be 1" in {
    Σp(4)(1) should be (1)
  }

  "Σp(4)(2)" should "be 17" in {
    Σp(4)(2) should be (17)
  }

  "Σp(4)(3)" should "be 98" in {
    Σp(4)(3) should be (98)
  }

  "toString" should "be (n(n + 1)(-n^3 + 16n^2 - 6n + 6)) / 30" in {
    Σp(4).toString should be ("(n(n + 1)(-n^3 + 16n^2 - 6n + 6)) / 30")
  }
/*
  "aoeu" should "snth" in {
    val σp = Σp(4)

    (0 to 3)
        .map(σp(_))
        .foldLeft(σp.toString) { (lhs, rhs) =>
          s"${lhs}\n${rhs}"
        }
  }
  */
}
