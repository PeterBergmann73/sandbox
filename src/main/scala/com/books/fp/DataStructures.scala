package com.books
package fp


object DataStructures {

  // finds the position of a target value within a sorted array
  def binarySearch[T](as: Array[T], key: T, gt: (T, T) => Boolean): Int = {

    @annotation.tailrec
    def go(low: Int, high: Int): Int = {
      if (low > high) -1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)

        if (a == key) mid2
        else if (greater) go(low = low, high = mid2 - 1)
        else go(low = mid2 + 1, high = high)
      }
    }

    go(low = 0, high = as.length - 1)
  }

}
