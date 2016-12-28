package com.books
package testingscala

import com.BaseSpec

class MatchersTest extends BaseSpec {

  "string matcher" in {
    "brr" should startWith("b")
  }

  "buffer matches" in {
    (0.9 - 0.8) should beEqualToTolerance(right = 0.1, tolerance = 0.01)
  }

  "pending test" in {
    pending
  }

}
