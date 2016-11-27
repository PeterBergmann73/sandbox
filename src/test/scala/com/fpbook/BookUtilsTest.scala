package com.fpbook

import com.BaseSpec

class BookUtilsTest extends BaseSpec {

  "divisible by 3 and/or 5" in {
    BookUtils.divisibleBy(3)(10) shouldBe false
    BookUtils.divisibleBy(5)(10) shouldBe true
    BookUtils.divisibleBy3And5(10) shouldBe false
    BookUtils.divisibleBy3Or5(10) shouldBe true
  }

}
