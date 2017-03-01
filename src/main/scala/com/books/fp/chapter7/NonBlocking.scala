package com.books
package fp
package chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}


object NonBlocking {

  sealed trait Future[+A] {

    private[chapter7] def apply(k: A => Unit): Unit

  }


  type Par[+A] = ExecutorService => Future[A]


  object Par {


    def run[A](es: ExecutorService)(p: Par[A]): A = {
      // A mutable, thread-safe reference to use for storing the result.
      // See the java.util.concurrent.atomic package for more information about
      // these classes.
      val ref = new java.util.concurrent.atomic.AtomicReference[A]

      // A java.util.concurrent.CountDownLatch allows threads
      // to wait until its countDown method
      // is called a certain number of times.
      // Here the countDown method will be
      // called once when we’ve received the
      // value of type A from p , and we want
      // the run implementation to block
      // until that happens.
      val latch = new CountDownLatch(1)

      // Asynchronously set the result, and decrement the latch
      p(es) {
        a => ref.set(a); latch.countDown()
      }

      // Block until the `latch.countDown` is invoked asynchronously
      latch.await()

      // Once we've passed the latch,
      // we know `ref` has been set, and return its value
      ref.get
    }


    // `unit` is represented as a function that returns a `UnitFuture`,
    // which is a simple implementation of `Future`
    // that just wraps a constant value.
    // It doesn't use the `ExecutorService` at all.
    // It's always done and can't be cancelled.
    // Its `get` method simply returns the value that we gave it.
    def unit[A](a: A): Par[A] = {
      es =>
        val f = new Future[A] {
          override private[chapter7] def apply(k: (A) => Unit) = k(a)
        }

        f
    }


    // a non-strict version of 'unit'
    def delay[A](a: => A): Par[A] = {
      es =>
        val f = new Future[A] {
          override private[chapter7] def apply(k: (A) => Unit) = k(a)
        }

        f
    }


    /**
      * Helper function, for evaluating an action
      * asynchronously, using the given `ExecutorService`.
      */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {def call: Unit = r })


    // fork is using ExecutorService
    // eval forks off evaluation of a and returns
    // immediately. The callback will be invoked
    // asynchronously on another thread.
    def fork[A](a: => Par[A]): Par[A] = {
      es =>
        new Future[A] {
          override private[chapter7] def apply(k: (A) => Unit) =
            eval(es)(a(es)(k))
        }
    }

  }

}
