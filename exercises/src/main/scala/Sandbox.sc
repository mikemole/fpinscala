object Sandbox {
  val l = List(1,2)                               //> l  : List[Int] = List(1, 2)
  l.drop(-1)                                      //> res0: List[Int] = List(1, 2)
  Nil.drop(1)                                     //> res1: List[Nothing] = List()
  
  // Exercise 3.8
  // The result is that a List is constructed just like
  // the apply method does.
  // It seems to indicate that the data constructors
  // on list are just a special case of foldRight.
  import fpinscala.datastructures._
  List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
                                                  //> res2: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
  
}