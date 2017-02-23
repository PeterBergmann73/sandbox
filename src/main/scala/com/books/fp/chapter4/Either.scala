package com.books
package fp
package chapter4


trait Either[+E, +A] {


  // p. 61. EXERCISE 7: Implement versions of map, flatMap, orElse, and map2 on
  // Either that operate on the Right value.
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case l: Left[E]  => l
      case r: Right[A] => Right(f(r.value))
    }
  }


  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case l: Left[E]  => l
      case r: Right[A] =>
        val r1: Either[EE, B] = f(r.value)

        r1 match {
          case l2: Left[E]  => l2
          case r2: Right[B] => r2
        }
    }
  }


  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case l: Left[E]  => b
      case r: Right[A] => r
    }
  }


  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case l: Left[E]  => l
      case r: Right[A] =>
        b match {
          case l2: Left[EE] => l2
          case r2: Right[B] => Right[C](f(r.value, r2.value))
        }
    }
  }

}


final case class Left[+L](value: L) extends Either[L, Nothing]


final case class Right[+R](value: R) extends Either[Nothing, R]


object Either {


  // p. 62. EXERCISE 8: Implement sequence and traverse for Either.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)


  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
  }

}



