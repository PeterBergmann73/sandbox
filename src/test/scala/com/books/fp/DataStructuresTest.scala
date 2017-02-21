package com.books
package fp

import com.util.Specs2Spec


class DataStructuresTest extends Specs2Spec {

  "binary search" in {
    val arr1 = (0 until 3).toArray
    val arr2 = (0 until 4).toArray
    val arr3 = Array(1, 3, 2)

    def gr(a: Int, b: Int) = a > b

    def rep(a: Array[Int]) = a.foreach {
      v =>
        val ind = DataStructures.binarySearch(a, v, gr)

        if (ind < 0) {
          sys.error(s"illegal index $ind of element $v in the array ${a.toSeq}")
        }
    }

    rep(arr1)
    rep(arr2)
    rep(arr3) must throwAn("illegal index -1 of element 2 in the array")
  }

}
