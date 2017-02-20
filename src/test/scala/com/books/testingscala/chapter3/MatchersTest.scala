package com.books
package testingscala
package chapter3


import com.BaseSpec
import com.books.testingscala.chapter1.Album


class MatchersTest extends BaseSpec {

  "string matcher" in {
    "brr" should startWith("b")
    "brr" should endWith("r")
    "brr" should include("rr")
  }

  "buffer matches" in {
    (0.9 - 0.8) should beEqualToTolerance(right = 0.1, tolerance = 0.01)
    (0.9 - 0.8) should be(0.1 +- 0.01)
  }

  "pending test" in {
    pending
  }

  "collection matchers" in {
    List() should be('empty)
    (1 until 8) should contain(7)
    (1 to 9) should have length 9
  }

  "map matchers" in {
    val map = Map(
      "Jimmy Page" -> "Led Zeppelin",
      "Sting" -> "The Police",
      "Aimee Mann" -> "Til\' Tuesday"
    )

    map should contain key "Sting"
    map should contain value "Led Zeppelin"
    map should not contain key("Brian May")
  }

  "exception handling" in {
    val thrown: RuntimeException = the [RuntimeException] thrownBy Album("brr", 1910, None)
    info("exception is thrown")
    thrown.getMessage should include("No Artist provided")
  }

}
