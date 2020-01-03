package ch1

sealed abstract class Tree[A]

case class Leaf[A](value: A) extends Tree[A]

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def numberOfLeaves[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Node(left, right) => numberOfLeaves(left) + numberOfLeaves(right)
  }

  def relabel[A](t: Tree[A], i: Int): (Tree[(Int, A)], Int) = t match {
    case Leaf(a) => (Leaf(i, a), i + 1)
    case Node(left, right) => {
      val (l1, i1) = relabel(left, i)
      val (r1, i2) = relabel(right, i1)
      (Node(l1, r1), i2)
    }
  }

  type WithCounter[A] = Int => (A, Int)

  implicit class RichWithCounter[A](f: WithCounter[A]) {
    def next[B](g: A => WithCounter[B]): WithCounter[B] = i => {
      val (r, i1) = f(i)
      g (r)(i1)
    }
  }

  def pure[A](a: A): WithCounter[A] = i => (a, i)

  def relabel2[A](t: Tree[A]): WithCounter[Tree[(A, Int)]] = t match {
    case Leaf(x) => i => (Leaf((x, i)), i + 1)
    case Node(l, r) => relabel2(l) next { ll =>
      relabel2(r) next { rr =>
        pure(Node(ll, rr))
      }
    }
  }

  type State[S, A] = S => (A, S)

  def pure2[S, A](a: A): State[S, A] = i => (a, i)

  def next2[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] = i => {
    val (r, i1) = f(i)
    g (r)(i1)
  }
}