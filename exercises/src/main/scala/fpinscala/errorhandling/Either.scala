package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => Right(f(a))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => f(a)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(a) => Right(a)
 }

// def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
//   (this,b) match {
//   case (Right(a),Right(b)) => Right(f(a,b))
//   case (Left(e),_) => Left(e)
//   case (_, Left(e)) => Left(e)
// }
def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
  for {
    a <- this
    b1 <- b
  } yield f(a, b1)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    es match {
    case Nil => Right(Nil)
    // Q: What is going on with the (_ :: _)?
    // Where are we passing that append function to?
    // A: Oh wait... it's going to map2.
    case a::as => (f(a) map2 traverse(as)(f))(_ :: _) 
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 
    traverse(es)((e) => e)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
  
  /*
   * Exercise 4.8
   * I would change map2 to collect all errors rather than mkPerson because
   * map2 currently kicks out as soon as the first error is encountered.
   * mkPerson is agnostic to this.
   * I don't think you need to create a new datatype rather than Either
   * because you could just use an Either[List[String],Person].
   * They would all have to be changed to append errors to a list of strings
   * rather than just returning the first error.
   */

}