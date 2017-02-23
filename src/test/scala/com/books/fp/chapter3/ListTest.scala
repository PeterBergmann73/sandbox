package com.books
package fp
package chapter3

import com.util.Specs2Spec


class ListTest extends Specs2Spec {

  val l: MyList[Int] = MyList(1, 2, 3)

  "append" in {
    val r: MyList[Int] = MyList(4, 5, 6)

    val appended: MyList[Int] = l.append(r)
    appended must be_==(MyList(1, 2, 3, 4, 5, 6))
  }

  "add1" in {
    val r = MyList.add1(l)
    r must be_==(MyList(2, 3, 4))
  }

  "Nil" in {
    MyList[Int]() must_== Nil
  }
}
