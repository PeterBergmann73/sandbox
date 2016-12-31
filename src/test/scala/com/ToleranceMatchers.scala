package com

import org.scalatest.matchers.{MatchResult, Matcher}

trait ToleranceMatchers {

  def beEqualToTolerance(right: Double, tolerance: Double): Matcher[Double] = {
    new Matcher[Double] {
      def apply(left: Double) = MatchResult(
        left >= right - tolerance && left <= right + tolerance,
        s"$left is not equal to $right within the tolerance $tolerance",
        s"$left is equal to $right within the tolerance $tolerance"
      )
    }

  }

}
