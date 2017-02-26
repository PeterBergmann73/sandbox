package com.books
package fp
package chapter3


import scala.collection.immutable.Queue


trait Tree[+A] {

  def +[B >: A <% Ordered[B]](elem: B): Tree[B]

  def ++[B >: A <% Ordered[B]](bst: Tree[B]): Tree[B]

  def -[B >: A <% Ordered[B]](elem: B): (Option[B], Tree[B])

  def exists(p: A => Boolean): Boolean

  def contains[B >: A <% Ordered[B]](e: B): Boolean

  def filter[B >: A <% Ordered[B]](p: A => Boolean): Tree[B] = filterAcc[B](NilTree)(p)

  def filterAcc[B >: A <% Ordered[B]](acc: Tree[B])(p: A => Boolean): Tree[B]

  def flatMap[B <% Ordered[B]](f: A => Tree[B]): Tree[B]

  def map[B <% Ordered[B]](f: A => B): Tree[B]

  def inOrder[B](z: B)(f: (A, B) => B): B

  def preOrder[B](z: B)(f: (A, B) => B): B

  def postOrder[B](z: B)(f: (A, B) => B): B

  def levelOrder[B](z: B)(f: (A, B) => B): B

  def withLeft[B >: A <% Ordered[B]](newLeft: Tree[B]): Tree[B]

  def withRight[B >: A <% Ordered[B]](newRight: Tree[B]): Tree[B]

  def orElse[B >: A <% Ordered[B]](tree: Tree[B]): Tree[B]

  def minChild[B >: A <% Ordered[B]]: Tree[B] = minChildAcc[B](this)

  def minChildAcc[B >: A <% Ordered[B]](acc: Tree[B]): Tree[B]

  def toList: List[A] = preOrder(List[A]())(_ :: _).reverse


  // p. 47. EXERCISE 25: Write a function size that counts the number of nodes in a tree.
  def size[B >: A <% Ordered[B]](t: Tree[B]): Int = {
    t match {
      case NilTree         => 0
      case bt: TreeNode[B] => 1 + size(bt.left) + size(bt.right)
    }
  }


  // p.47 EXERCISE 27: Write a function depth that returns the maximum path length
  // from the root of a tree to any leaf.
  def depth[B >: A <% Ordered[B]](t: Tree[B]): Int = {
    t match {
      case NilTree         => 0
      case tn: TreeNode[B] =>
        import scala.math.max
        1 + max(depth(tn.left), depth(tn.right))
    }
  }


  // p. 48. EXERCISE 29: Generalize size, maximum, depth, and map, writing a new
  // function fold that abstracts over their similarities. Reimplement them in terms of
  // this more general function.
  final def fold[B >: A <% Ordered[B]](z: B)(f: (B, B) => B): B = this match {
    case NilTree => z
    case l       =>
      l.toList.fold(z)(f)
  }


  // p. 47 EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int].
  def maximum(t: Tree[Int]): Option[Int] = {
    t match {
      case NilTree           => None
      case tn: TreeNode[Int] =>

        import scala.math.max

        val fromBranches: Option[Int] = (maximum(tn.left), maximum(tn.right)) match {
          case (Some(l), Some(r)) => Some(scala.math.max(l, r))
          case (l, r)             => l.orElse(r)
        }

        val res = fromBranches.fold(tn.elem) {
          fb =>
            max(tn.elem, fb)
        }

        Some(res)
    }
  }
}


private[chapter3] case object NilTree extends Tree[Nothing] {
  def +[B <% Ordered[B]](elem: B) = Tree(elem)

  def ++[B <% Ordered[B]](bst: Tree[B]) = bst

  def -[B <% Ordered[B]](elem: B) = (None, NilTree)

  def flatMap[B <% Ordered[B]](f: Nothing => Tree[B]): Tree[B] = NilTree

  def map[B <% Ordered[B]](f: Nothing => B): Tree[B] = NilTree

  def exists(p: Nothing => Boolean) = false

  def contains[B <% Ordered[B]](e: B) = false

  def filterAcc[B <% Ordered[B]](acc: Tree[B])(p: Nothing => Boolean) = acc

  def inOrder[B](z: B)(f: (Nothing, B) => B) = z

  def preOrder[B](z: B)(f: (Nothing, B) => B) = z

  def postOrder[B](z: B)(f: (Nothing, B) => B) = z

  def levelOrder[B](z: B)(f: (Nothing, B) => B) = z

  def withLeft[B <% Ordered[B]](newLeft: Tree[B]) = newLeft

  def withRight[B <% Ordered[B]](newRight: Tree[B]) = newRight

  def orElse[B <% Ordered[B]](tree: Tree[B]) = tree

  def minChildAcc[B <% Ordered[B]](acc: Tree[B]) = acc

  override def toString = "[]"
}


private[chapter3] case class TreeNode[A <% Ordered[A]](elem: A, left: Tree[A], right: Tree[A]) extends Tree[A] {

  def +[B >: A <% Ordered[B]](newElem: B): TreeNode[_ >: A <: B] = {
    if (newElem < elem) {
      withLeft(left + newElem)
    } else if (newElem > elem) {
      withRight(right + newElem)
    } else {
      this
    }
  }

  def ++[B >: A <% Ordered[B]](bst: Tree[B]): Tree[B] = bst.preOrder[Tree[B]](this) {
    (e, acc) =>
      acc + e
  }

  def -[B >: A <% Ordered[B]](e: B): (Option[B], Tree[B]) =
    if (e < elem) left - e match {
      case (opt, l) => (opt, withLeft(l))
    } else if (e > elem) right - e match {
      case (opt, r) => (opt, withRight(r))
    } else (Some(elem), (left, right) match {
      case (NilTree, NilTree) => NilTree
      case (l, NilTree)       => l
      case (NilTree, r)       => r
      case (l, r)             => right.minChild match {
        case NilTree             => r.withLeft(l)
        case TreeNode(min, _, _) => TreeNode(min, l, (r - min)._2)
      }
    })

  def exists(p: A => Boolean): Boolean = p(elem) || left.exists(p) || right.exists(p)

  def contains[B >: A <% Ordered[B]](e: B): Boolean = exists(_ == e)

  def filterAcc[B >: A <% Ordered[B]](acc: Tree[B])(p: A => Boolean): Tree[B] =
    right.filterAcc(left.filterAcc(if (p(elem)) acc + elem else acc)(p))(p)

  def flatMap[B <% Ordered[B]](f: A => Tree[B]): Tree[B] = preOrder(f(elem))((e, acc) => acc ++ f(e))


  // p.47. EXERCISE 28: Write a function map, analogous to the method of the same
  // name on List, that modifies each element in a tree with a given function.
  def map[B <% Ordered[B]](f: A => B): Tree[B] = preOrder[Tree[B]](Tree(f(elem)))((e, acc) => acc + f(e))

  def inOrder[B](z: B)(f: (A, B) => B): B = right.inOrder(f(elem, left.inOrder(z)(f)))(f)

  def preOrder[B](z: B)(f: (A, B) => B): B = right.preOrder(left.preOrder(f(elem, z))(f))(f)

  def postOrder[B](z: B)(f: (A, B) => B): B = f(elem, right.postOrder(left.postOrder(z)(f))(f))

  def levelOrder[B](z: B)(f: (A, B) => B): B = {
    def recurse(acc: B, queue: Queue[Tree[A]]): B = queue match {
      case Queue() => acc
      case h +: t  => h match {
        case NilTree           => recurse(acc, t)
        case TreeNode(e, l, r) => recurse(f(e, acc), t.enqueue(l).enqueue(r))
      }
    }

    recurse(z, Queue(this))
  }

  def withLeft[B >: A <% Ordered[B]](newLeft: Tree[B]) = {
    TreeNode(elem, newLeft, right)
  }

  def withRight[B >: A <% Ordered[B]](newRight: Tree[B]) = {
    TreeNode(elem, left, newRight)
  }

  def minChildAcc[B >: A <% Ordered[B]](acc: Tree[B]): Tree[A] = left.minChildAcc(this)

  def orElse[B >: A <% Ordered[B]](tree: Tree[B]): Tree[A] = this

  override def toString: String = s"$elem[l=$left, r=$right]"
}

object Tree {

  def apply[A <% Ordered[A]](): Tree[A] = NilTree

  def apply[A <% Ordered[A]](elem: A, elems: A*): Tree[A] = {
    def recurse(elems: List[A], bst: Tree[A]): Tree[A] =
      if (elems.isEmpty) bst
      else recurse(elems.tail, bst + elems.head)

    recurse(elems.toList, TreeNode(elem, NilTree, NilTree))
  }
}


//object BalancedTree {
//
//
//  // p. 47. EXERCISE 25: Write a function size that counts the number of nodes in a tree.
//  def size[A](t: Tree[A]): Int = {
//    t match {
//      case Nil                 => 0
//      case bt: BalancedTree[A] => 1 + size(bt.left) + size(bt.right)
//    }
//  }
//
//
//  // p. 47 EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int].
//  def maximum(t: Tree[Int]): Option[Int] = {
//
//    t match {
//      case Nil                   => None
//      case bt: BalancedTree[Int] =>
//        (maximum(bt.left), maximum(bt.right)) match {
//          case (Some(l), Some(r)) => Some(max(l, r))
//          case (l, r)             => l.orElse(r)
//        }
//    }
//  }
//
//
//  // p.47 EXERCISE 27: Write a function depth that returns the maximum path length
//  // from the root of a tree to any leaf.
//  def depth[A](t: Tree[A]): Int = {
//    t match {
//      case Nil                 => 0
//      case bt: BalancedTree[A] => 1 + max(depth(bt.left), depth(bt.right))
//    }
//  }
//
//
//  // p.47. EXERCISE 28: Write a function map, analogous to the method of the same
//  // name on List, that modifies each element in a tree with a given function.
//  def map[A, B](t: Tree[A], op: A => B): Tree[B] = {
//    t match {
//      case Nil                 => Nil
//      case bt: BalancedTree[A] =>
//        BalancedTree[B](data = op(bt.data), left = map(bt.left, op), right = map(bt.right, op))
//    }
//  }
//
//
//  // p. 48. EXERCISE 29: Generalize size, maximum, depth, and map, writing a new
//  // function fold that abstracts over their similarities. Reimplement them in terms of
//  // this more general function.
//  def fold[A, B](t: Tree[A])(f: A => B): B = {
//    t match {
//      case Nil                 => None
//      case bt: BalancedTree[A] => BalancedTree[B] (fold(l)(f)(g), fold(r)(f)(g))
//    }
//  }
//
//  def sizeViaFold[A](t: Tree[A]): Int = {
//    fold(t)(a => 1)(1 + _ + _)
//  }
//
//  def maximumViaFold(t: Tree[Int]): Int = {
//    fold(t)(a => a)((l, r) => max(l, r))
//  }
//
//  def depthViaFold[A](t: Tree[A]): Int = {
//    fold(t)(a => 1)((l, r) => 1 + max(l, r))
//  }
//
//
//  /*
//Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this:
//
//type mismatch;
//  found   : fpinscala.datastructures.Branch[B]
//  required: fpinscala.datastructures.Leaf[B]
//     fold(t)(a => Leaf(f(a)))(Branch(_,_))
//                                    ^
//
//This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the
//annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument
//to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
//infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat
//common to define helper functions that simply call the corresponding data constructors but give the less specific
//result type:
//
//  def leaf[A](a: A): Tree[A] = Leaf(a)
//  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
//*/
//  def mapViaFold[A, B](t: Tree[A])(op: A => B): Tree[B] = {
//    fold(t)(a => Leaf(op(a)): Tree[B])(Branch(_, _))
//  }
//}
