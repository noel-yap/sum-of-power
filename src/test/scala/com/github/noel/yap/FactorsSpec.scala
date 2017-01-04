package com.github.noel.yap

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FactorsSpec extends FlatSpec with Matchers {
  "Factor pairs of 12" should "be (1, 12), (2, 6), (3, 4), (4, 3), (6, 2), (12, 1), (-1, -12), (-2, -6), (-3, -4), (-4, -3), (-6, -2), (-12, -1)" in {
    Factors.factorPairs(12L) should contain only ((1, 12), (2, 6), (3, 4), (4, 3), (6, 2), (12, 1), (-1, -12), (-2, -6), (-3, -4), (-4, -3), (-6, -2), (-12, -1))
  }

  "Factor pairs of 4" should "be (1, 4), (2, 2), (4, 1), (-1, -4), (-2, -2), (-4, -1)" in {
    Factors.factorPairs(4L) should contain only ((1, 4), (2, 2), (4, 1), (-1, -4), (-2, -2), (-4, -1))
  }

  "Factor pairs of -1" should "be (-1, 1), (1, -1)" in {
    Factors.factorPairs(-1L) should contain only ((-1, 1), (1, -1))
  }
}
