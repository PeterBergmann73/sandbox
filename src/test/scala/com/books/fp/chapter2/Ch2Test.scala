package com.books
package fp
package chapter2

import com.util.Specs2Spec


class Ch2Test extends Specs2Spec {

  "factorial" in {
    import Ch2.factorial

    factorial(0) must be_==(1)
    factorial(1) must be_==(1)
    factorial(2) must be_==(2)
    factorial(3) must be_==(6)
  }


  "fibonacci" in {
    import Ch2.fibonacci

    val f0 = fibonacci(0)
    f0 must be_==(0)

    val f1 = fibonacci(1)
    f1 must be_==(1)

    val f2 = fibonacci(2)
    f2 must be_==(1)

    val f3 = fibonacci(3)
    f3 must be_==(2)
  }


  "is sorted" in {
    val arr1 = (0 to 3).toArray
    val arr2 = Array(1, 3, 2)

    import Ch2.isSorted

    def gr(a: Int, b: Int): Boolean = a > b

    isSorted(arr1, gr) must beTrue
    isSorted(arr2, gr) must beFalse
  }

}
