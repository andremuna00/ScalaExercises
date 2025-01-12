// ------------------------------------------------------------------------------------------------------------------------------------
// Functional programs with state 

// Type ST as a trait 
object ST_as_trait: 
 trait ST[S, A]:  
  self =>
    def run(s:S) : (A,S)
 
    def flatMap[B](f: A => ST[S, B]): ST[S, B] =
        new ST[S,B] { def run(s:S) = { val (a, ns) = self.run(s); f(a).run(ns) } }

    def map[B](f: A => B): ST[S, B] =
        new ST[S,B] { def run(s:S) = { val (a, ns) = self.run(s); (f(a), ns) } }

 object ST: 
    def apply[S,A](a : => A) = new ST[S,A] { def run(s:S) = (a,s) }


// ST as a state class 
case class ST[S, A](run: S => (A,S)):
 
    def flatMap[B](f: A => ST[S, B]): ST[S, B] =
        new ST[S,B]( (s:S) => { val (a,ns) = run(s); f(a).run(ns) } )

    def map[B](f: A => B): ST[S, B] =
        new ST[S,B]( (s:S) => { val (a,ns) = run(s); (f(a),ns) } )

object ST: 
    def apply[S,A](a : A) =  new ST[S,A] ((s:S) => (a,s))


// The evaluator with state to operation counter 

trait Term
case class Const(n:Int) extends Term
case class Add(t:Term, u:Term) extends Term
case class Div(t:Term, u:Term) extends Term

// Naive version

def eval(t:Term) : ST[Int,Double] = 
    new ST((s : Int) => 
     t match
        case Const(d) => (s,d)
        case Add(t,u) => 
            val (a, sa) = eval(t).run(s) 
            val (b, sb) = eval(u).run(sa) 
            (a+b,sb) 
	        
        case Div(t,u) => 
            val (a, sa) = eval(t).run(s) 
            val (b, sb) = eval(u).run(sa)
            (a/b,sb+1))

// With monadic operators

def eval_(t:Term) : ST[Int,Double] = 
    def tick : ST[Int,Unit] = new ST((s:Int) => ((),s+1))
    t match
        case Const(d) => ST(d)
        case Add(t,u) => eval_(t).flatMap(a => eval_(u).map(b => a+b))      
        case Div(t,u) => eval_(t).flatMap(a => eval_(u).flatMap(b => tick.flatMap(_ =>ST(a/b) )))

// with for comprehensions 

def eval_f(t:Term) : ST[Int,Double] = 
    def tick : ST[Int,Unit] = new ST((s:Int) => ((),s+1))
    t match
        case Const(d) => ST(d)
        case Add(t,u) => 
            for 
              a <- eval_f(t)
              b <- eval_f(u)
            yield a+b   
        case Div(t,u) => 
            for 
                a <- eval_f(t)
                b <- eval_f(u)
                _ <- tick
            yield a/b

// ------------------------------------------------------------------------------------------------------------------------------------
// Case study: random number generator

trait RNG:
  def nextInt: (Int, RNG) 


case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

// Using RNG

def double(rng: RNG): (Double, RNG) =
    val (i, r) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), r)

def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match
      case (i,rng2) => (i%2==0,rng2)

def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)


def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then
      (List(), rng)
    else
      val (x, r1)  = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)

// A tail-recursive solution
def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if count <= 0 then
        (xs, r)
      else
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
    go(count, rng, List())

// ------------------------------------------------------------------------------------------------------------------------------------
// Generalizing the desing - a generic Rand type

type Rand[A] = ST[RNG,A]

object Rand:
    def get[A](rng:RNG)(r:Rand[A]): A = r.run(rng)._1  // extract the generated random number 
    

// Using Rand - new versions of previous functions 

def nextInt : Rand[Int] = new ST((_:RNG).nextInt)

def double : Rand[Double] = 
    nextInt.map(i => (i / (Int.MaxValue.toDouble + 1)))

def double_f : Rand[Double] = 
    for (i <- nextInt) yield i / (Int.MaxValue.toDouble + 1)

def intDouble : Rand[(Int,Double)] = 
    for (i <- nextInt; d <- double) yield (i,d)

def ints(count: Int) : Rand[List[Int]] = 
    if count <= 0 then
        ST(List[Int]())
    else 
        for 
          x <- nextInt
          xs <- ints(count -1)
        yield (x::xs) 

def ints2(count: Int): Rand[List[Int]] =
    def go(count: Int, xs: List[Int]): Rand[List[Int]] =
      if count <= 0 then
        ST(xs)
      else 
        for 
            x <- nextInt
            l <- go(count-1,x::xs)
        yield l
    go(count, List())


extension[S,A](sta : ST[S, A]) 
    def map2[B,C](stb : ST[S,B])(f:(A,B) => C) : ST[S,C] =
        new ST[S,C]( (s:S) => { val (a,sa) = sta.run(s); val (b,sb) = stb.run(sa);  (f(a,b),sb) } )

def unit[A](a: A): Rand[A] = ST(a)

def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((st,acc) => st.map2(acc)(_ :: _))


// ------------------------------------------------------------------------------------------------------------------------------------
// ST as a monad 

trait Monad[F[_]]: 
    def unit[A](a: => A) : F[A]

    extension [A](ma:F[A])
        def flatMap[B](f: A => F[B]) : F[B]
        def map[B](f:A => B) : F[B] = 
            ma.flatMap(a => unit(f(a)))
        def map2[B,C](mb: F[B])(f: (A,B) => C) : F[C] =
            ma.flatMap(a => mb.map(b => f(a,b)))


type IntState[A] = ST[Int,A]

given Monad[IntState] with 
    def unit[A](a: => A): IntState[A] = ST(a)
    extension [A](fa: IntState[A])
      override def flatMap[B](f: A => IntState[B]) = fa.flatMap(f)


given StMonad[S] : Monad[[x] =>> ST[S, x]] with 
    def unit[A](a: => A): ST[S, A] = ST(a)
    extension [A](fa: ST[S, A])
      override def flatMap[B](f: A => ST[S, B]) = fa.flatMap(f)


