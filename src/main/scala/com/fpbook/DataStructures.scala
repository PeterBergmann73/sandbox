package com.fpbook


object DataStructures {

  def binarySearch[T](a: Array[T], key: T, comparator: (T, T) => Boolean): Int = {

    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val d = a(mid2)
        if (d == key) mid2
        else if (comparator(d, key))
          go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }

    go(0, 0, a.length - 1)
  }

}
