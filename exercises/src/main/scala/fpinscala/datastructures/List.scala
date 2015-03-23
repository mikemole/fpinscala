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

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_,b) => b + 1) // better answer uses acc rather than b
  }

  // NOTE: Using @annotation.tailrec is a great way to ensure
  // a function is tail recursive.
  // The foldRight above is not tail recursive.  You can see it
  // because evaluation is started on a stack frame but must
  // then wait for the next element to be evaluated.
  // So for foldLeft, we want to avoid this by performing the evaluation
  // immediately before making the recursive call
  // (i.e. apply the function f (which can immediately return)
  // before having to descend into the next call to foldLeft.
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z, h))(f)
  }
  
  // Sum in terms of foldLeft
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  
  // Product in terms of foldLeft
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  
  // length in terms of foldLeft
  def length3[A](l: List[A]): Int = 
    foldLeft(l, 0)((acc, _) => acc + 1)
    
  // Reverse of a list
  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil:List[A])((xs, x) => Cons(x, xs))
    
  // foldLeft in terms of foldRight
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???
    
  // foldRight in terms of foldLeft
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = ???
  
  // Concatenate a list of lists into a single list
  def concat[A](l: List[List[A]]): List[A] = 
    // foldLeft(l, Nil:List[A])(append(_, _))
    foldRight(l, Nil:List[A])(append)

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}