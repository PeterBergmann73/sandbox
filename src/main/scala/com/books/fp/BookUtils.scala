package com.books
package fp


object BookUtils {

  type Pred[A] = A => Boolean

  def even: Pred[Int] = divisibleBy(2)

  def positive: Pred[Int] = _ > 0

  def odd: Int => Boolean = not(even)

  def nonPositive: Int => Boolean = not(positive)

  def abs(n: Int): Int = if (n < 0) -n else n

  def absolute[A](f: A => Int): A => Int = n => abs(f(n))

  def divisibleBy(k: Int): Pred[Int] = _ % k == 0

  def divisibleBy3And5: Pred[Int] = divisibleBy(and, 3, 5)

  def divisibleBy3Or5: Pred[Int] = divisibleBy(or, 3, 5)

  def divisibleBy(f: (Boolean, Boolean) => Boolean, a: Int, b: Int): Pred[Int] = {
    lift(f)(divisibleBy(a), divisibleBy(b))
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => {
      b =>
        f(a, b)
    }
  }

  def curry[A, B, C, D](f: (A, B, C) => D): (A, B) => C => D = {
    case (a, b) =>
      c =>
        f(a, b, c)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) =>
      f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a =>
      f(g(a))
  }

  def lift[A, B, C, D](f: (B, C) => D)(g: A => B, h: A => C): A => D = {
    a =>
      f(g(a), h(a))
  }

  def divBox: (Box => Double, Box => Double) => Box => Double = {
    lift[Box, Double, Double, Double](_ / _)
  }

  def aspectRatio: Box => Double = divBox(_.height, _.width)

  def lift3[A, B, C, D, E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E = {
    a =>
      val curried: (B, C) => D => E = curry(f)
      val lifted: D => E = lift(curried)(g, h)(a)
      lifted(i(a))
  }

  def fib(n: Int): Int = {

    @annotation.tailrec
    def loop(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, curr, prev + curr)
    }

    loop(n, 0, 1)
  }

  def sqrt(n: Double): Double = {
    require(n >= 0.0, "positive value expected")

    if (n <= 1e-14) {
      0.0
    } else {
      def f(x: Double) = (x * x) - n

      iterateWhile(2.0)(x => x - f(x) / (2 * x), x => f(x).abs > 1e-14)
    }
  }

  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A = {
    var res = a
    while (p(res)) {
      val calc = f(res)
      res = calc
    }

    res
  }

  def not[A](f: Pred[A]): Pred[A] = a => !f(a)

  def or: (Boolean, Boolean) => Boolean = _ || _

  def and: (Boolean, Boolean) => Boolean = _ && _

}
