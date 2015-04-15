package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] =
  //  if (this != None) this else ob
    map(Some(_)) getOrElse ob
    
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) this else None
    case None => None
  }
  def filter2(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case None => None
  }
  def filter3(f: A => Boolean): Option[A] = 
    flatMap(a => if (f(a)) Some(a) else None)
}

// General thoughts: It's often much easier at this point
// to implement functions using pattern matching rather
// than in terms of other functions.

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
    
  // What's going through my mind right now...
  // Trying to disconnect my association of flatMap to List only.
  // Anger... also anger.
  // Q: How can one think about these operations like map,
  // flatMap, etc. on an Option so that your head is out of the weeds.
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs map(x => math.pow(x - m, 2))))
  }

  // I continue to see pattern matching as the easy way out...
  def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a,b) match {
    case (None,_) => None
    case (_,None) => None
    case (Some(a),Some(b)) => Some(f(a, b))
  }
    
  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b flatMap (bb => Some(f(aa, bb))))
  
  // While I'm in the moment.  Let me gather my thoughts about the difference
  // between map2 and map2_2 because it's all so clear now...
  // The big question is why did I think I had to wrap f in a Some() in
  // map2_2.  I thought, if I "flatten" it out, then I end up with a C
  // (i.e. the result of f(A,B) and need to return an Option[C], so
  // I wrapped it in a Some, which at that point is adding a little
  // unnecessary overhead because I've guaranteed that at that point
  // I'll have a C.
  // In map2, though, I can rely on "map" to deal with the logic of
  // applying my function, f, to the value in the B and wrapping it
  // in an Option, thus eliminating the need to unwrap it and re-wrap it.
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  // Combine a list of Options into one Option containing a list
  // of all the Some values in the original list.  If the original
  // list contains None even once, the result of the function
  // should be None; otherwise the result should be Some
  // with a list of all the values
  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    a match {
    case Nil => Some(Nil)
    // WTF??  Kind of makes sense if you code it backwards... grumble
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }
  
  // The version using foldRight was annoying...
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def Try[A](a: => A): Option[A] = 
    try Some(a)
    catch { case e: Exception => None }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a.foldRight[Option[List[B]]](Some(Nil))((x,xs) => map2(f(x),xs)(_ :: _))
    
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}