package com.books
package fp
package chapter3


import scala.math._


sealed trait Tree[+A]


case class Leaf[A](value: A) extends Tree[A]


case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  // p. 47. EXERCISE 25: Write a function size that counts the number of nodes in a tree.
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }


  // p. 47 EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int].
  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(_)      => 1
      case Branch(l, r) => max(maximum(l), maximum(r))
    }
  }


  // p.47 EXERCISE 27: Write a function depth that returns the maximum path length
  // from the root of a tree to any leaf.
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + max(depth(l), depth(r))
    }
  }


  // p.47. EXERCISE 28: Write a function map, analogous to the method of the same
  // name on List, that modifies each element in a tree with a given function.
  def map[A, B](t: Tree[A], op: A => B): Tree[B] = {
    t match {
      case Leaf(a)      => Leaf(op(a))
      case Branch(l, r) => Branch(map(l, op), map(r, op))
    }
  }


  // p. 48. EXERCISE 29: Generalize size, maximum, depth, and map, writing a new
  // function fold that abstracts over their similarities. Reimplement them in terms of
  // this more general function.
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(a)      => g(f(a))
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(a => 1)(1 + _ + _)
  }

  def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)(a => a)((l, r) => max(l, r))
  }

  def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(a => 1)((l, r) => 1 + max(l, r))
  }


  /*
Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this:

type mismatch;
  found   : fpinscala.datastructures.Branch[B]
  required: fpinscala.datastructures.Leaf[B]
     fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                    ^

This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the
annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument
to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat
common to define helper functions that simply call the corresponding data constructors but give the less specific
result type:

  def leaf[A](a: A): Tree[A] = Leaf(a)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
*/
  def mapViaFold[A, B](t: Tree[A])(op: A => B): Tree[B] = {
    fold(t)(a => Leaf(op(a)): Tree[B])(Branch(_, _))
  }
}
