package fpinscala.laziness

import Stream._
trait Stream[+A] {
  
  def toList: List[A] =
    this match {
    case Empty => List()
    case Cons(h,t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => Empty
  }
  
  // Q: I'm having trouble figuring out when the compiler
  // doesn't have enough information for type inference
  // vs. a problem with my code.  Tips?
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else Empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h,t) => p(h) && t)

  // Figured out the type issues on this one. Yay!
  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h,t) => cons(f(h), t))
    
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t)
    
  // I'll admit I'm a little puzzled about this one because:
  // 1. Why switch the Stream's type from A to B
  // 2. What is B>:A in layman's terms
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)
    
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) { 
      case Cons(h,t) => Some((f(h()), t())) 
      case _ => None
    }
  
  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this,n) {
      case (Cons(h,t), n) if (n > 0) => Some((h(), (t(), n-1)))
      case _ => None
    }
  
  // How does this implicit tuple conversion below work?
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case (Cons(h,t)) if p(h()) => Some(h(), t())
      case _ => None
    }
  
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold(this,s2) {
      case (Cons(h1,t1),Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
      case _ => None
    }
  
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(this,s2) {
      case (Cons(h1,t1),Cons(h2,t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1,t1),_) => Some((Some(h1()), None), (t1(), empty))
      case (_,Cons(h2,t2)) => Some((None, Some(h2())), (empty, t2()))
      case _ => None
    }
   
  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))
    
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(empty)(s => Some(a, s))

  val ones: Stream[Int] = Stream.cons(1, ones)
  val onesViaUnfold: Stream[Int] = unfold(empty)(s => Some(1, s))
  
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n+1))
  
  val fibs: Stream[Int] = {
    def fibsInt(a: Int, b: Int): Stream[Int] =
      cons(a, fibsInt(b, a+b))
    fibsInt(0, 1)
  }
  val fibsViaUnfold: Stream[Int] =
    unfold((0,1)) { case (f0,f1) => Some(f0, (f1,f0+f1)) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case _ => Empty
  }
    
}