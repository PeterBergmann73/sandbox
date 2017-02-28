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


  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
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
  def fork[A](a: => Par[A]): Par[A] =  {
    es => es.submit {
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
  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }


  // p.107.
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit())((a, _) => f(a))


  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    as.map(fork)



  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    val xx: List[Par[B]] = ps.map(asyncF(f))
  }
}
