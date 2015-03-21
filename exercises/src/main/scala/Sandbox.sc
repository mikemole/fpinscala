object Sandbox {
  val l = List(1,2)                               //> l  : List[Int] = List(1, 2)
  l.drop(-1)                                      //> res0: List[Int] = List(1, 2)
  Nil.drop(1)                                     //> res1: List[Nothing] = List()
}