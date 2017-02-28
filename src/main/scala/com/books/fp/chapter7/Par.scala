package com.books
package fp
package chapter7


import java.util.concurrent._

import language.implicitConversions


/**
  * Created by slava on 28.02.17.
  */
object Par {

  type Par[A] = ExecutorService => Future[A]


  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  // `unit` is represented as a function that returns a `UnitFuture`,
  // which is a simple implementation of `Future`
  // that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all.
  // It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)


  private final case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    // `map2` doesn't evaluate the call to `f` in a separate logical thread,
    // in accord with our design choice of having `fork`
    // be the sole function in the API for controlling parallelism.
    // We can always do `fork(map2(a,b)(f))`
    // if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af: Future[A] = a(es)
      val bf: Future[B] = b(es)
      // This implementation of `map2` does _not_ respect timeouts.
      // It simply passes the `ExecutorService` on to both `Par` values,
      // waits for the results of the Futures `af` and `bf`,
      // applies `f` to them, and wraps them in a `UnitFuture`.
      // In order to respect timeouts,
      // we'd need a new `Future` implementation
      // that records the amount of time spent evaluating `af`,
      // then subtracts that time from the available
      // time allocated for evaluating `bf`.
      UnitFuture(f(af.get, bf.get))
    }
  }


  // This is the simplest and most natural implementation of `fork`,
  // but there are some problems with it
  // --for one,
  // the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool,
  // or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism.
  // Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation,
  // and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] = {
    es =>
      es.submit {
        new Callable[A] {
          def call = a(es).get
        }
      }
  }


  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))


  // p. 107. EXERCISE 7.4
  // This API already enables a rich set of operations.
  // Here’s a simple example: using lazyUnit ,
  // write a function to convert any function A => B to one
  // that evaluates its result asynchronously.
  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }


  // p.107.
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit())((a, _) => f(a))


  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }


  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)


  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }


  // p. 101. Split method map2 into 2 functions
  // one creating a parallel computation that waits for the result of two other computations,
  // and the other is combining their results using some function.
  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = {
    es => UnitFuture((fa(es).get, fb(es).get))
  }


  def map1[A, B](fa: Par[A])(f: A => B): Par[B] = {
    es => UnitFuture(f(fa(es).get))
  }


  // p. 101. EXERCISE 5 (optional): Implement product and map as primitives, then
  // define map2 in terms of them.
  //  def map2Decomposed[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
  //      val x1: Par[(A, B)] = product(a, b)
  //      type AB = (A, B)
  //      map1[AB, C](x1)(f)
  //  }


  // p. 103. EXERCISE 8: Implement parFilter, which filters elements of a list in
  // parallel.
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = l map asyncF((a: A) => if (f(a)) List(a) else List())
    val xx1: Par[List[List[A]]] = sequence(pars)
    map1(xx1)(_.flatten)
  }


  // p. 110. EXERCISE 14: Try writing a function to choose between two forking
  // computations based on the result of an initial computation. Can this be
  // implemented in terms of existing combinators or is a new primitive required?
  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    es =>
      val x = a(es).get
      if (x) ifTrue(es) else ifFalse(es)
  }


  // p. 111. EXERCISE 15: Implement choiceN and then choice in terms of choiceN
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es =>
      val ind: Int = run(es)(n).get
      run(es)(choices(ind))
  }


  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))
  }


  // p. 112.
  def choiceMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] = {
    es =>
      val k: A = run(es)(a).get
      run(es)(choices(k))
  }


  // p. 112. EXERCISE 17: Implement this new primitive chooser, then use it to
  // implement choice and choiceN.
  def chooser[A, B](a: Par[A])(choices: A => Par[B]): Par[B] = {
    es =>
      val k: A = run(es)(a).get
      run(es)(choices(k))
  }


  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = chooser(a)(f)


  def join[A](a: Par[Par[A]]): Par[A] = {
    es =>
      run(es)(run(es)(a).get())
  }
}
