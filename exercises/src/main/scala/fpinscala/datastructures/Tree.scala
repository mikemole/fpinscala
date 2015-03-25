package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // TODO: Try to make tail recursive
  //@annotation.tailrec
  def size[A](tree: Tree[A]): Int = 
    tree match {
    case null => 0
    case leaf:Leaf[A] => 1
    case branch:Branch[A] => size(branch.left) + size(branch.right) + 1
  }


}