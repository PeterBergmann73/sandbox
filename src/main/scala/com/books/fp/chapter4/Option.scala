package com.books
package fp
package chapter4


sealed trait Option[+A] {

  // p. 55. EXERCISE 1: We'll explore when you'd use each of these next. But first, as an
  // exercise, implement all of the above functions on Option. As you implement
  // each function, try to think about what it means and in what situations you'd use it.
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None    => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case l    => l
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case l    => l
  }
}


case class Some[+A](get: A) extends Option[A]


case object None extends Option[Nothing]


object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    val length = xs.length

    if (length == 0) None
    else Some(xs.sum / length)
  }


  // p. 56. EXERCISE 2: Implement the variance function (if the mean is m, variance
  // is the mean of math.pow(x - m, 2), see definition) in terms of mean and
  // flatMap.
  def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => scala.math.pow(x - m, 2))))


  // p. 59. EXERCISE 3: is an instance of a more general bothMatch pattern. Write a
  // generic function map2, that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too.
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      ra <- a
      rb <- b
    } yield {
      f(ra, rb)
    }
  }


  // p. 59. EXERCISE 5: Write a function sequence, that combines a list of Options
  // into one option containing a list of all the Some values in the original list. If the
  // original list contains None even once, the result of the function should be None,
  // otherwise the result should be Some with a list of all the values.
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))


  // p. 60. EXERCISE 6: Implement function traverse
  // returning None if applying it to any element of the list returns None.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil    => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }



}
