package com.books
package fp
package chapter3

import com.util.Specs2Spec


class ListTest extends Specs2Spec {

  val l: List[Int] = List(1, 2, 3)

  "append" in {
    val r: List[Int] = List(4, 5, 6)

    val appended: List[Int] = l.append(r)
    appended must be_==(List(1, 2, 3, 4, 5, 6))
  }

  "add1" in {
    val r = List.add1(l)
    r must be_==(List(2, 3, 4))
  }

  "Nil" in {
    List[Int]() must_== Nil
  }
}
