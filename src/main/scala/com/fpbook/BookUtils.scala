package com.fpbook


object BookUtils {

  def even: Pred[Int] = divisibleBy(2)

  def positive: Pred[Int] = _ > 0

  def odd: Int => Boolean = not(even)

  def nonPositive: Int => Boolean = not(positive)

  def abs(n: Int): Int = if (n < 0) -n else n

  def absolute[A](f: A => Int): A => Int = n => abs(f(n))

  type Pred[A] = A => Boolean

  def divisibleBy(k: Int): Pred[Int] = _ % k == 0

  def divisibleBy3And5: Pred[Int] = divisibleBy(and, 3, 5)

  def divisibleBy3Or5: Pred[Int] = divisibleBy(or, 3, 5)

  def divisibleBy(f: (Boolean, Boolean) => Boolean, a: Int, b: Int): Pred[Int] = {
    lift(f, divisibleBy(a), divisibleBy(b))
  }

  private def not[A](f: A => Boolean): A => Boolean = n => !f(n)

  private def lift[A](f: (Boolean, Boolean) => Boolean, g: Pred[A], h: Pred[A]): Pred[A] = {
    a =>
      f(g(a), h(a))
  }

  private def or: (Boolean, Boolean) => Boolean = _ || _

  private def and: (Boolean, Boolean) => Boolean = _ && _

}
