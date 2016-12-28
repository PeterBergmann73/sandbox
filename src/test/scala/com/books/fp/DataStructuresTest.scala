package com.books
package fp

import com.BaseSpec


class DataStructuresTest extends BaseSpec {

  "binary search" in {
    val arr1 = (0 until 3).toArray
    val arr2 = (0 until 4).toArray

    def gr(a: Int, b: Int) = a > b

    def rep(a: Array[Int]) = a.map {
      v =>
        val ind = DataStructures.binarySearch(a, v, gr)
        ind should be (v)
    }

    rep(arr1)
    rep(arr2)
  }

}
