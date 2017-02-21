package com.books
package fp
package chapter2


object Ch2 {

  def factorial(x: Int): Int = {

    @scala.annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else go(n = n - 1, acc = n * acc)
    }

    go(x, 1)
  }


  // p. 22, EXERCISE 1 (optional): Write a function to get the nth Fibonacci number. The
  // first two Fibonacci numbers are 0 and 1, and the next number is always the sum of
  // the previous two. Your definition should use a local tail-recursive function.
  def fibonacci(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else go(n = n - 1, prev = curr, curr = prev + curr)
    }

    go(n, 0, 1)
  }


  // p. 28 EXERCISE 2: Implement isSorted, which checks whether an Array[A] is
  //  sorted according to a given comparison function.
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    val n = as.length


    @scala.annotation.tailrec
    def go(i: Int): Boolean = {
      if(i >= n - 1) true
      else if (gt(as(i), as(i + 1))) false
      else go(i + 1)
    }

    go(0)
  }


  // p. 29 EXERCISE 3 (hard): Implement partial1 and write down a concrete usage
  // of it.
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b =>
      f(a, b)
  }


  // p. 30 EXERCISE 4 (hard): Let's look at another example, currying, which converts a
  // function of N arguments into a function of one argument that returns another
  // function as its result.
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => {
      b =>
        f(a, b)
    }
  }


  // p. 30 EXERCISE 5 (optional): Implement uncurry, which reverses the
  // transformation of curry.
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) =>
      f(a)(b)
  }


  // p. 31 EXERCISE 6: Implement the higher-order function that composes two
  // functions.
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a =>
      f(g(a))
  }

}
