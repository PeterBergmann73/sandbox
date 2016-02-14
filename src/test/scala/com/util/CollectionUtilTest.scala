package com.util

import com.BaseSpec


class CollectionUtilTest extends BaseSpec {

  "safeToMap" should {

    "no duplicates" in {
      val pairs = Seq("one" -> 1, "two" -> 2, "three" -> 3)
      val mapped = CollectionUtil.safeToMap(pairs)
      val res = Map("one" -> 1, "three" -> 3, "two" -> 2)

      mapped shouldEqual res
    }

  }

}
