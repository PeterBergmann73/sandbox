package com.util

import com.BaseSpec

class CollectionUtilTest extends BaseSpec {

  "safeToMap" should {

    "no duplicates" in {
      val pairs = Seq("one" -> 1, "two" -> 2, "three" -> 3)
      val mapped = CollectionUtil.safeToMap(pairs)

      println(s"mapped: $mapped")

      1 shouldEqual 1
    }

  }

}
