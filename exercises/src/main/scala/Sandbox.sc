object Sandbox {
  val l = List(1,2)
  l.drop(-1)
  Nil.drop(1)
  
  // Exercise 3.8
  // The result is that a List is constructed just like
  // the apply method does.
  // It seems to indicate that the data constructors
  // on list are just a special case of foldRight.
  import fpinscala.datastructures._
  List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
}