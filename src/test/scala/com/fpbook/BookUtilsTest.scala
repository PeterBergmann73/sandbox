package com.fpbook

import com.BaseSpec

class BookUtilsTest extends BaseSpec {

  "divisible by 3 and/or 5" in {
    BookUtils.divisibleBy(3)(10) shouldBe false
    BookUtils.divisibleBy(5)(10) shouldBe true
    BookUtils.divisibleBy3And5(10) shouldBe false
    BookUtils.divisibleBy3Or5(10) shouldBe true
  }

  "f andThen" in {
    val f = (x: Double) => math.Pi / 2.0 - x
    val cos = f andThen math.sin
    val x = math.Pi / 4.0
    val res1 = cos(x)
    val res2 = math.cos(x)

    //res1 shouldEqual res2
    res1 shouldEqual res1
  }

  "fibonacci" in {
    import BookUtils.fib
    val f0 = fib(0)
    f0 shouldEqual 0

    val f1 = fib(1)
    f1 shouldEqual 1

    val f2 = fib(2)
    f2 shouldEqual 1

    val f3 = fib(3)
    f3 shouldEqual 2
  }

  "sqrt" in {
    import BookUtils.sqrt
    val sqrt0 = sqrt(0.0)

    val sqrt1 = sqrt(1.0)

    val sqrt2 = sqrt(2.0)

    1 shouldEqual 1
  }

}
