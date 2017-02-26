package com.books
package fp
package chapter4

import com.books.fp.chapter5.Stream
import com.util.Specs2Spec


class StreamTest extends Specs2Spec {

  val l0 = List(1, 2, 3)
  val l: List[Int] = List(1, 2, 3, 4, -3, -2, -1)
  val s: Stream[Int] = Stream(l: _*)


  "toList" in {
    val l1 = s.toList
    l1 must_== l
  }


  "takeWhile" in {
    val tw = s.takeWhile(_ < 4)
    val l1 = tw.toList
    l1 must_== l0

    val tw1 = s.takeWhileViaFoldRight(_ < 4)
    val l2 = tw1.toList
    l2 must_== l0
  }

}
