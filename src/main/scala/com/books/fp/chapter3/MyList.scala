package com.books
package fp
package chapter3


sealed trait MyList[+A] {

  def head: A = this match {
    case Nil    => sys.error("head of empty list")
    case Cons(h, _) => h
  }

  // p. 40. EXERCISE 2: Implement the function tail for "removing" the first element
  // of a List. Notice the function takes constant time.
  def tail: MyList[A] = this match {
    case Nil    => sys.error("tail of empty list")
    case Cons(h, t) => t
  }


  // p. 40. EXERCISE 3: Generalize tail to the function drop, which removes the first
  // n elements from a list.
  final def drop(n: Int): MyList[A] = {
    // The implementation below is not tail recursive as it calls drop method from the other list
    //    if (n <= 0) this
    //    else {
    //      this match {
    //        case Nil        => Nil
    //        case Cons(h, t) => t.drop(n - 1)
    //      }
    //    }

    // we have to put an additional function
    @scala.annotation.tailrec
    def go(l: MyList[A], i: Int): MyList[A] = {
      if (i <= 0) l
      else {
        l match {
          case Nil    => l
          case Cons(_, t) => go(t, i - 1)
        }
      }
    }

    go(this, n)
  }


  // p. 40. EXERCISE 4: Implement dropWhile,10 which removes elements from the
  // List prefix as long as they match a predicate. Again, notice these functions take
  // time proportional only to the number of elements being dropped—we do not need
  // to make a copy of the entire List.
  final def dropWhile(p: A => Boolean): MyList[A] = {
    // The implementation below is not tail recursive as it calls dropWhile method from the other list
    //    case Nil        => this
    //    case Cons(h, t) =>
    //      if (!p(h)) this
    //      else t.dropWhile(p)

    // we have to put an additional function
    @scala.annotation.tailrec
    def go(l: MyList[A]): MyList[A] = {
      l match {
        case Nil    => l
        case Cons(h, t) =>
          if (!p(h)) l
          else go(t)
      }
    }

    go(this)
  }


  // p. 41. EXERCISE 5: Using the same idea, implement the function setHead for
  // replacing the first element of a List with a different value.
  // Here we have to use S SuperClass of A to avoid "covariant parameter in contrvariant position" error.
  def setHead[S >: A](h: S): MyList[S] = this match {
    case Nil    => sys.error("setting head to empty list")
    case Cons(_, t) => Cons(h, t)
  }


  // p. 42. EXERCISE 6: Not everything works out so nicely. Implement a function,
  // init, which returns a List consisting of all but the last element of a List. So,
  // given List(1,2,3,4), init will return List(1,2,3).
  def init: MyList[A] = this match {
    case Nil          => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)       => Cons(h, t.init)
  }


  // Please, note, 'op' takes first "A" and then "B"
  def foldRight[B](z: B)(op: (A, B) => B): B = {
    // The implementation below is not tail recursive as it calls function on the recursion
    //    this match {
    //      case Nil         => z
    //      case Cons(x, xs) => op(x, xs.foldRight(z)(op))
    //    }

    // we have to go through the reversion first
    // the implementation below is copied from the List from the standard library,
    // but it does not compile.
    reverse.foldLeft(z)((right, left) => op(left, right))
  }


  // p. 44. EXERCISE 7: Can product implemented using foldRight immediately
  // halt the recursion and return 0.0 if it encounters a 0.0? Why or why not?
  // Consider how any short-circuiting might work if you call foldRight with a
  // large list.

  /*
  *  No, this is not possible! The reason is because _before_ we ever call our function, `op`,
  *  we evaluate its argument,
  *  which in the case of `foldRight` means traversing the list all the way to the end.
  *  We need _non-strict_ evaluation to support early termination---we discuss this in chapter 5.
*/


  // p. 44. EXERCISE 8: See what happens when you pass Nil and Cons themselves to
  // foldRight, like this: foldRight(List(1,2,3),
  // Nil:List[Int])(Cons(_,_)).13 What do you think this says about the
  // relationship between foldRight and the data constructors of List?

  /*
  * We get back the original list! Why is that?
  * As we mentioned earlier, one way of thinking about what `foldRight` "does"
  * is it replaces the `Nil` constructor of the list with the `z` argument,
  * and it replaces the `Cons` constructor with
  * the given function, `op`. If we just supply `Nil` for `z` and `Cons` for `op`,
  * then we get back the input list.

  * foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
  * Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
  * Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
  * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
  * Cons(1, Cons(2, Cons(3, Nil)))
*/


  // p. 44. EXERCISE 9: Compute the length of a list using foldRight.
  def length: Int = foldRight(0)((_, acc) => acc + 1)


  // p. 44 EXERCISE 10: foldRight is not tail-recursive and will StackOverflow
  // for large lists. Convince yourself that this is the case, then write another general
  // list-recursion function, foldLeft that is tail-recursive, using the techniques we
  // discussed in the previous chapter.
  // Please, note, 'op' takes first "B" and then "A"
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    // The implementation below is not tail recursive as it calls drop method from the other list
    //    this match {
    //      case Nil        => z
    //      case Cons(h, t) => t.foldLeft(op(z, h))(op)
    //    }

    MyList.foldLeft(this, z)(op)
  }


  // p. 45. EXERCISE 12: Write a function that returns the reverse of a list (so given
  // List(1,2,3) it returns List(3,2,1)). See if you can write it using a fold.
  def reverse: MyList[A] = foldLeft(MyList[A]())((acc, h) => Cons(h, acc))


  // p. 45. EXERCISE 14: Implement append in terms of either foldLeft or
  // foldRight.
  /*
  * `append` simply replaces the `Nil` constructor of the first list
  * with the second list,
  * which is exactly the operation performed by `foldRight`.
*/
  def append[S >: A](r: MyList[S]): MyList[S] = foldRight(r)(Cons(_, _))


  // we cannot use here length == 0,
  // as it will cause crash on null in foldLeft.
  def isEmpty: Boolean = this match {
    case Nil => true
    case _       => false
  }


  // p.45. EXERCISE 15 (hard): Write a function that concatenates a list of lists into a
  // single list. Its runtime should be linear in the total length of all lists. Try to use
  // functions we have already defined.


  // p. 46. EXERCISE 18: Write a function map, that generalizes modifying each element
  // in a list while maintaining the structure of the list.
  def map[B](op: A => B): MyList[B] = foldRight(Nil: MyList[B])((a, list) => Cons(op(a), list))


  // p.46. EXERCISE 19: Write a function filter that removes elements from a list
  // unless they satisfy a given predicate.
  def filter(p: A => Boolean): MyList[A] = foldRight(Nil: MyList[A])((a, list) => if (p(a)) Cons(a, list) else list)


  // p. 46. EXERCISE 20: Write a function flatMap, that works like map except that
  // the function given will return a list instead of a single result, and that list should be
  // inserted into the final resulting list.
  def flatMap[B](op: A => MyList[B]): MyList[B] = MyList.concat(map(op))


  // p. 46. EXERCISE 21: Can you use flatMap to implement filter?
  def filterViaFlatMap(p: A => Boolean): MyList[A] = flatMap(a => if (p(a)) MyList(a) else Nil)
}


case object Nil extends MyList[Nothing]

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A]


object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case _       => ints.foldLeft(0)(_ + _)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case _       => ds.foldLeft(1.0)(_ * _)
  }

  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](l: MyList[A], z: B)(f: (B, A) => B): B = l match {
    case Nil    => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldRight[A, B](l: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def reverse[A](l: MyList[A]): MyList[A] = foldLeft(l, MyList[A]())((acc, h) => Cons(h, acc))

  def append[A](l: MyList[A], r: MyList[A]): MyList[A] = foldRight(l, r)(Cons(_, _))

  def concat[A](l: MyList[MyList[A]]): MyList[A] =
    foldRight(l, Nil: MyList[A])(append)

  def map[A, B](l: MyList[A])(f: A => B): MyList[B] =
    foldRight(l, Nil: MyList[B])((h, t) => Cons(f(h), t))

  def flatMap[A, B](l: MyList[A])(f: A => MyList[B]): MyList[B] =
    concat(map(l)(f))

  // p. 45. EXERCISE 16: Write a function that transforms a list of integers by adding 1
  // to each element. (Reminder: this should be a pure function that returns a new List!)
  def add1(l: MyList[Int]): MyList[Int] = l.foldRight(Nil: MyList[Int])((h, t) => Cons(h + 1, t))

  // p. 46. EXERCISE 17: Write a function that turns each value in a List[Double]
  // into a String.
  def toString(l: MyList[Int]): MyList[String] = l.foldRight(Nil: MyList[String])((h, t) => Cons(h.toString, t))


  @scala.annotation.tailrec
  def startsWith[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil)                 => true
      case (Nil, _)                 => false
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 != h2) false
        else startsWith(t1, t2)
    }
  }


  // p. 47. EXERCISE 24 (hard): As an example, implement hasSubsequence for
  // checking whether a List contains another List as a subsequence. For instance,
  // List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as
  // subsequences, among others.
  @scala.annotation.tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    sub match {
      case _ if startsWith(sup, sub) => true
      case Cons(h, t) => hasSubsequence(t, sub)
    }
  }
}
