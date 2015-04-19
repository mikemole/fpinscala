package awkcomonads

object FizzBuzz {
  
  type Pred = Int => Boolean
  type ToStr = Int => Option[String]
  
  def isFizzBuzz: Pred = _ % 15 == 0              //> isFizzBuzz: => awkcomonads.FizzBuzz.Pred
  def isFizz: Pred = _ % 3 == 0                   //> isFizz: => awkcomonads.FizzBuzz.Pred
  def isBuzz: Pred = _ % 5 == 0                   //> isBuzz: => awkcomonads.FizzBuzz.Pred

  def f1(p: Pred, s: String): ToStr  =
    (i: Int) => if (p(i)) Some(s) else None       //> f1: (p: awkcomonads.FizzBuzz.Pred, s: String)awkcomonads.FizzBuzz.ToStr
  
  def fizzBuzz = f1(isFizzBuzz, "fizzbuzz")       //> fizzBuzz: => awkcomonads.FizzBuzz.ToStr
  def fizz = f1(isFizz, "fizz")                   //> fizz: => awkcomonads.FizzBuzz.ToStr
  def buzz = f1(isBuzz, "buzz")                   //> buzz: => awkcomonads.FizzBuzz.ToStr
  
//  def g(ps: List[ToStr])(i: Int): Option[String] = //sys.error("todo")
//    ps.map(_(i)).filter( _ != None ).head

  def g(ps: List[ToStr])(i: Int): Option[String] =
    ps match {
    case h::t => h(i) orElse g(t)(i)
    case _ => None
    }                                             //> g: (ps: List[awkcomonads.FizzBuzz.ToStr])(i: Int)Option[String]

//  @annotation.tailrec
//  def g(ps: List[ToStr])(i: Int): Option[String] =
//    ps.dropWhile(_.isEmpty).headOption
  
  def intToStr(i: Int): String =
    g(fizzBuzz :: fizz :: buzz :: Nil)(i).getOrElse(i.toString)
                                                  //> intToStr: (i: Int)String
  
  //var toStrList( fizzBuzz, fizz, buzz )
  println((1 to 100).toList.map(intToStr))        //> List(1, 2, fizz, 4, buzz, fizz, 7, 8, fizz, buzz, 11, fizz, 13, 14, fizzbuzz
                                                  //| , 16, 17, fizz, 19, buzz, fizz, 22, 23, fizz, buzz, 26, fizz, 28, 29, fizzbu
                                                  //| zz, 31, 32, fizz, 34, buzz, fizz, 37, 38, fizz, buzz, 41, fizz, 43, 44, fizz
                                                  //| buzz, 46, 47, fizz, 49, buzz, fizz, 52, 53, fizz, buzz, 56, fizz, 58, 59, fi
                                                  //| zzbuzz, 61, 62, fizz, 64, buzz, fizz, 67, 68, fizz, buzz, 71, fizz, 73, 74, 
                                                  //| fizzbuzz, 76, 77, fizz, 79, buzz, fizz, 82, 83, fizz, buzz, 86, fizz, 88, 89
                                                  //| , fizzbuzz, 91, 92, fizz, 94, buzz, fizz, 97, 98, fizz, buzz)
  println( f1( isFizzBuzz, "FizzBuzz" )( 15 ) )   //> Some(FizzBuzz)
}