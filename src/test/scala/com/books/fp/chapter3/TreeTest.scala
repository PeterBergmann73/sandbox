package com.books
package fp
package chapter3

import com.util.Specs2Spec


class TreeTest extends Specs2Spec {

    "tree creation" in {
      val t1 = Tree(1)
      val t2 = t1 + 2
      val t3 = t2 + 3
      val t4 = t3 + 4
      val t5 = t4 + 5
      val t6 = t5 + 6

      ok
    }

}
