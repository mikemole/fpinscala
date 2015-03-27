package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  // TODO: Try to make tail recursive
  //@annotation.tailrec
  def size[A](tree: Tree[A]): Int = 
    tree match {
    case null => 0
    case leaf:Leaf[A] => 1
    case branch:Branch[A] => size(branch.left) + size(branch.right) + 1
  }

  // 3.26
  // Why no [A] after maximum??
  // A: That defines a new type
  def maximum(tree: Tree[Int]): Int = 
    tree match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  // 3.27
  def depth(tree: Tree[Int]): Int = 
    tree match {
    case Leaf(v) => 1
    case Branch(l,r) => (1 + depth(l)) max (1 + depth(r))
  }
  
  // 3.28
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = 
    tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
  
  // 3.29
  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B =
    tree match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  
  // 3.30
  def size2[A](tree: Tree[A]): Int = 
    fold(tree)(_ => 1)(1 + _ + _)
  
  // 3.30
  def maximum2(tree: Tree[Int]): Int =
    fold(tree)(a => a)((l,r) => l max r)
    
  // 3.30
  def depth2(tree: Tree[Int]): Int =
    fold(tree)(_ => 1)((l,r) => (1 + l) max (1 + r))

  // 3.30
  // The fact that you need to throw the :Tree[B] in there for
  // the compiler to figure out that you're returning a subtype
  // of Tree is... hopefully I can remember that nonsense.
  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    //fold(tree)(l => Leaf(f(l)): Tree[B])((l,r) => Branch(l,r))
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  
}