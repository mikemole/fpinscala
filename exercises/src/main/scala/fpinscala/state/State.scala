package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  
  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  // ACM Q - Always seem to have trouble with pulling variables
  // out of thin air with scala.  How exactly is this to be read?
  // Maybe the way to think about it is that you're defining a function that will
  // get that object passed to it when used.  You don't have it yet, but when the 
  // function is used, it'll be available then, so you just need to define what'll
  // happen when you get it.
  // I need an "a ha" moment with pushing these parameters through
  // my type definitions and transformations.
  // Also, I think this signature initially confuses me as I'm thinking
  // it's a class method, not an object method... I'm thinking, why am
  // I passing s?
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
                                       // <--- Here we're generally implementing map to return a Rand (i.e. RNG => (B, RNG))
    rng => {                           // <--- Here we're saying it's mapping a rng (where does rng come from) to...
      val (a, rng2) = s(rng)           // <--- Here we get to use that rng that we specified on the left by passing to our Rand[A]
      (f(a), rng2)                     // <--- and returning a tuple, applying our function to "a", with the next state
    }
    // One more pass at trying to state what's happening...
    // You start with an s: Rand[A] which is a function that takes an instance of type RNG and returns a tuple
    // representing a randomly generated A and the RNG's next state.
    // You also get a function which takes an instance of type A and returns an instance of type B.
    // Your goal is to re-use the Rand[A] function, but map the As to Bs at the each step of the way.
    // In other words, please return a function with takes an instance of type RNG and returns a tuple
    // of (B, RNG) where RNG is the next state.
    // So, we say, ok, it's going to be a function that is provided an RNG instance, "rng";
    // we'll have that shit. Given that we have that shit, we're going to take it and return a tuple.
    // We can't do it in one step, so we'll write an inline function which does some work, but ultimately
    // returns what we want.  Again, given that we've got that "rng", here's the function that should
    // be performed with that rng...
    // That function is going to be to use that "rng" in the s: Rand[A] which we already know
    // takes a RNG to an (A,RNG).  So we'll use the "rng" there and get our A and our next state "rng2".
    // Then we'll apply the function, f, to a, and return the tuple.
    // It might be helpful to think of Rand like you think of "f", just another function, which is entirely accurate.
    // 
    // Ok... That's all good I suppose... But now why do we curry the f rather than pass it as a second parameter?
    // That still always bugs me...
    // Let's try it..
  def mapNoCurry[A,B](s: Rand[A], f: A => B): Rand[B] =
    rng => {
      val (a,rng2) = s(rng)
      (f(a),rng2)
    }
  // Ah ha, the answer is that without currying I'm not able to define
  // my "f" in terms of what comes back from my "s" above which is
  // much more convenient. Instead, I have to define another function
  // explicitly since it's completely independent.
  // I could be missing the point...
  def doubleViaMapNoCurry: Rand[Double] = {
    def f(i: Int): Double = i / (Int.MaxValue.toDouble + 1)
    mapNoCurry(nonNegativeInt, f)
  }
  
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // Question... Isn't the book's answer increasing the probability of 0?
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i >= 0) (i, r) 
    else if (i == Int.MinValue) nonNegativeInt(r)
    else (0 - i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val d = (i / (Int.MaxValue.toDouble + 1))
    (d, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i, rh) = rng.nextInt
      val (l, rt) = ints(count - 1)(rh)
      (i :: l, rt)
    } else {
      (List.empty, rng)
    }
  }
  
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a,rng2) = ra(rng)
      val (b,rng3) = rb(rng2)
      (f(a,b),rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(List[A]()))((h,t) => map2(h, t)(_::_))
    
  // I'm a little thrown off by the signature change...
  // which didn't actually change and seems to be necessary
  // to utilize List.fill, but still...
  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // Always kind of feel like flatMap should be called mapFlat
  // because it helps me visualize what's really happening,
  // but I suppose flatMap is more in line with English conventions
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (a,rnga) = f(rng)
      g(a)(rnga)
  }
  
  // This is really something worth analyzing further...
  // How this combinator lets us avoid handling
  // the RNG
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
}

// Having function types with noun names confuses me when using them.
// Explanation of case class here....
case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
  
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }
}
