package com.books
package fp
package chapter6


trait RNG {

  def nextInt: (Int, RNG)

}


object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG: Simple = Simple(newSeed)
      // The next state, which is an `RNG` instance created from the new seed.
      val n: Int = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }


  // p. 79. EXERCISE 1: Write a function to generate a random positive integer. Note:
  // you can use x.abs to take the absolute value of an Int, x. Make sure to handle
  // the corner case Int.MinValue, which doesn't have a positive counterpart.

  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }


  // p. 80. EXERCISE 2: Write a function to generate a Double between 0 and 1, not
  // including 1. Note: you can use Int.MaxValue to obtain the maximum positive
  // integer value and you can use x.toDouble to convert an Int, x, to a Double.
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    val rm = scala.math.abs(Int.MinValue.toDouble)
    val d = (i.toDouble + rm) / 2.0 / rm
    (d, r)
  }


  def boolean(rng: RNG): (Boolean, RNG) = {
    val (i, r) = rng.nextInt
    val b = i % 2 == 0
    (b, r)
  }


  // p. 80. EXERCISE 3: Write functions to generate an (Int, Double) pair, a
  // (Double, Int) pair, and a (Double, Double, Double) 3-tuple. You
  // should be able to reuse the functions you've already written.
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r0) = double(r)
    ((i, d), r0)
  }


  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }


  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }


  // p. 80. EXERCISE 4: Write a function to generate a list of random integers.
  // a simple non-recursive solution
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var r0 = rng

    val (i: List[Int], r: List[RNG]) = (0 to count).toList.map {
      _ =>
        val (i0, r1) = r0.nextInt
        r0 = r1
        (i0, r0)
    }.unzip

    (i, r.last)
  }


  // a tail-recursive solution
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(n: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (n <= 0) {
        (acc, r)
      } else {
        val (i0, r0) = r.nextInt
        go(n - 1, r0, i0 :: acc)
      }
    }

    go(count, rng, List())
  }


  type Rand[+A] = RNG => (A, RNG)


  val int: Rand[Int] = _.nextInt


  def unit[A](a: A): Rand[A] = rng => (a, rng)


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }


  // p. 81. EXERCISE 5: Use map to generate an Int between 0 and n, inclusive:
  def positiveMax(n: Int): Rand[Int] = {
    map(nonNegativeInt) {
      i =>
        val mod: Int = i % n
        mod + 1 // we have to include n as well
    }
  }


  // p. 82. EXERCISE 6: Use map to reimplement RNG.double in a more elegant way.
  def double2: Rand[Double] = {
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble)
  }


  // p. 82. EXERCISE 7: Unfortunately, map is not powerful enough to implement
  // intDouble and doubleInt from before. What we need is a new combinator
  // map2, that can combine two RNG actions into one using a binary rather than unary
  // function. Write its implementation and then use it to reimplement the intDouble
  // and doubleInt functions.
  // This implementation of map2 passes the initial RNG to the first argument
  // and the resulting RNG to the second argument. It's not necessarily wrong
  // to do this the other way around, since the results are random anyway.
  // We could even pass the initial RNG to both `f` and `g`, but that might
  // have unexpected results. E.g. if both arguments are `RNG.int` then we would
  // always get two of the same `Int` in the result. When implementing functions
  // like this, it's important to consider how we would test them for
  // correctness.
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      val c: C = f(a, b)
      (c, rngB)
    }
  }


  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))


  // p. 82.
  val randIntDouble: Rand[(Int, Double)] = {
    both(int, double)
  }


  // p. 82.
  val randDoubleToInt: Rand[(Double, Int)] = {
    both(double, int)
  }


  // p. 82.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }


  // p. 83. EXERCISE 9: Implement flatMap, then use it to reimplement
  // positiveInt.
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      val b: (B, RNG) = g(a)(r1)
      b
    }
  }


  // p. 83.
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        unit(mod + 1)
    }
  }


  // p. 83. EXERCISE 10: Reimplement map and map2 in terms of flatMap.
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }


  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
}
