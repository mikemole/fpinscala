package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
    
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_,xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("setHead of empty list")
    case Cons(_,xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    (l, n) match {
    case (Nil, _) => Nil
    case (l, n) if (n <= 0) => l
    case (Cons(_,xs), n) => drop(xs, n-1)
  }

  /**
   * Initial implementation drops any elements that don't match
   * the specified criteria.
   * I missed the "from the prefix" of the problem description.
   * @param l
   * @param f
   * @return
   */
//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
//    l match {
//    case Cons(x,xs) => 
//      if (f(x)) Cons(x, dropWhile(xs, f))
//      else dropWhile(xs, f)
//    case _ => l
//  }
  
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }
  
  // This function can't be implemented in constant time like tail
  // because we must traverse the entire list, building the return list
  // along the way, until we reach the last element.
  def init[A](l: List[A]): List[A] = 
    l match {
    case Nil => throw new UnsupportedOperationException("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(x,xs) => Cons(x, init(xs))
  }
  
  // Exercise 3.7
  // Q: Can product, using foldRight, immediately halt recursion and return
  //    0.0?
  // A: Product cannot halt recursion when a 0.0 is encountered because
  //    it does not have enough information about the significance of
  //    0.0 in relation to the passed function, f, during traversal.
  //    In other words, 0.0 has no intrinsic meaning for the traversal
  //    which occurs before any functions are applied.

  def length[A](l: List[A]): Int = sys.error("todo")

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}