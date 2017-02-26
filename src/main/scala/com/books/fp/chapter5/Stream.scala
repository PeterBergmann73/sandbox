package com.books
package fp
package chapter5


trait Stream[+A] {

  import Stream._

  // p. 69. EXERCISE 1: Write a function to convert a Stream to a List, which will
  // force its evaluation
  def toList: List[A] = {

    def go(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty      => acc
        case Cons(h, t) =>
          go(t(), h() :: acc)
      }
    }

    go(this, List()).reverse
  }


  // p. 69. EXERCISE 2: Write a function "take" for returning the first n elements of a
  // Stream.
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n == 1 => cons(h(), Empty)
      case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
      case _                    => empty
    }
  }


  // p. 70. EXERCISE 3: Write the function "takeWhile" for returning all starting
  // elements of a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) =>
        cons(h(), t().takeWhile(p))
      case _                    =>
        empty
    }
  }


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _          => z
    }


  // p. 70. EXERCISE 4: Implement forAll, which checks that all elements in the Stream
  // match a given predicate. Your implementation should Stream terminate the
  // traversal as soon as it encounters a non-matching value.
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }


  // p. 71. EXERCISE 5: Use foldRight to implement takeWhile. This will
  // construct a stream incrementally, and only if the values in the result are demanded
  // by some other expression.
  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] = {
    foldRight(empty[A]) {
      (h: A, t) =>
        if (f(h)) cons(h, t)
        else empty
    }
  }


  // p. 71. EXERCISE 6: Implement map, filter, append, and flatMap using
  // foldRight.
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }


  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]) {
      (h, t) =>
        if (p(h)) cons(h, t)
        else t
    }
  }


  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))


  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)


  // p. 74 EXERCISE 12: Use unfold to implement map, take, takeWhile, zip (as
  // in chapter 3), and zipAll. The zipAll function should continue the traversal as
  // long as either stream has more elements — it uses Option to indicate whether
  // each stream has been exhausted.
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _          => None
    }
  }


  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1)           => Some(h(), (empty, 0))
      case (Cons(h, t), n1) if n > 1 => Some(h(), (t(), n - 1))
      case _                         => None
    }
  }


  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _                    => None
    }
  }


  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold(this, s) {
      case (Cons(h, t), Cons(h1, t1)) => Some((f(h(), h1()), (t(), t1())))
      case _                          => None
    }
  }


  // special case zipWith
  def zip[B](s: Stream[B]): Stream[(A, B)] = {
    zipWith(s)((_, _))
  }


  def zipWithAll[B, C](s: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
    unfold(this, s) {
      case (Cons(h, t), Cons(h1, t1)) => Some(f(Some(h()), Some(h1())) -> (t(), t1()))
      case (Empty, Cons(h, t))        => Some(f(None, Some(h())) -> (empty[A], t()))
      case (Cons(h, t), Empty)        => Some(f(Some(h()), None) -> (t(), empty[B]))
      case (Empty, Empty)             => None
    }
  }


  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s)((_, _))


  // p. 74. EXERCISE 13 (hard): implement startsWith using functions you've
  // written. It should check if one Stream is a prefix of another.
  def startsWith[A](s: Stream[A]): Boolean =
  zipAll(s).takeWhile(_._2.isDefined).forAll {
    case (a, b) => a == b
  }

}


case object Empty extends Stream[Nothing]


final case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }


  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }


  def ones: Stream[Int] = cons(1, ones)


  // p.73 EXERCISE 7: Generalize ones slightly to the function constant which
  // returns an infinite Stream of a given value.
  def constant(c: Int): Stream[Int] = cons(c, constant(c))


  // p. 73 EXERCISE 8: Write a function that generates an infinite stream of integers,
  // starting from n, then n + 1, n + 2, etc.
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))


  // p. 73 EXERCISE 9: Write a function fibs that generates the infinite stream of
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  def fibs: Stream[Int] = {

    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }

    go(0, 1)
  }


  // p. 73. EXERCISE 10: We can write a more general stream building function. It takes
  // an initial state, and a function for producing both the next state and the next value
  // in the generated stream. It is usually called unfold:
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty
    }
  }


  // p. 73. EXERCISE 11: Write , , fibs from constant, and ones in terms of unfold.
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }


  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(n => Some(n, n + 1))
  }


  def constantViaUnfold[A](c: A): Stream[A] = {
    unfold(c)(c => Some(c, c))
  }

}
