import scala.util.control.NonFatal
import scala.util.{Either => _}
import scala.util.Random

extension[A](o: Option[A])
    def map2[B,C](o2: Option[B])(f: (A,B) => C): Option[C] = 
        o match
            case None => None
            case Some(a) => o2 match
                case Some(b) => Some(f(a,b))
                case None => None
    def map2_short[B,C](o2: Option[B])(f: (A,B) => C): Option[C] = 
        o.flatMap(a => o2.map(b => f(a,b)))

def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case Some(h)::t=> sequence(t).map(x=>h::x)
    case None :: _ => None
    case Nil => Some(Nil)

def sequence_flatmap[A](as: List[Option[A]]): Option[List[A]] = as match
    case h::t=> h.flatMap(a => sequence_flatmap(t).map(x=>a::x))
    case Nil => Some(Nil)

def sequence_fold[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a,acc)=>a.map2(acc)((x,y)=>x::y))
    
def traverse[A,B](as: List[A])(f: A=>Option[B]): Option[List[B]] =
    sequence(as.map(f))

enum Either[+E, +A]:
    case Left(value: E)
    case Right(value: A)

def mean(xs: List[Double]): Either[String, Double] =
    if xs.isEmpty then Either.Left("mean of empty list!")
    else Either.Right(xs.sum / xs.length)


def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Either.Right(x / y)
    catch case NonFatal(t) => Either.Left(t)

extension[E, A](o: Either[E,A])
    def map[B](f:A=>B): Either[E,B]=
        o match
            case Either.Left(v) => Either.Left(v)
            case Either.Right(v2) => Either.Right(f(v2))
    def flatMap[EE>:E, B](f:A=>Either[EE,B]): Either[EE,B]=
        o match
            case Either.Left(v) => Either.Left(v)
            case Either.Right(v2) => f(v2)
    def orElse[EE>:E, AA>:A](b: Either[EE, AA]):Either[EE, AA] =
        o match
            case Either.Left(_) => b
            case Either.Right(k) => Either.Right(k)
    def map2 [EE>:E, B, C](o2: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
        for 
            a<-o
            b<-o2
        yield f(a,b)    


def sequenceEither[E,A](as : List[Either[E, A]]): Either[E, List[A]] =
    as match
        case x::xs => x.flatMap(h=>sequenceEither(xs).map(k=>h::k))
        case Nil => Either.Right(Nil)
        
def traverse[E, A, B](as: List[B])(f: B=>Either[E, A]) : Either[E, List[A]] =
    sequenceEither(as.map(f))


enum Tree[+A]:
    case Leaf(value: A)
    case Branch(value: A, left: Tree[A], right: Tree[A])
    def size:Int = this match
        case Branch(_,l,r) => 1+l.size+r.size
        case Leaf(_) => 1
    def depth:Int = this match
        case Leaf(_) => 0
        case Branch(_,l,r) => 1 + (l.depth.max(r.depth))
    def map[B](f: A => B) : Tree[B] = 
        this match
            case Leaf(a) => Leaf(f(a))
            case Branch(value, left, right) => Branch(f(value), left.map(f), right.map(f))
    def fold[B](f: A=>B)(g: (B,B,B)=>B):B =
        this match
            case Leaf(value) => f(value)
            case Branch(value, left, right) => g(f(value), left.fold(f)(g), right.fold(f)(g))
    def size_fold:Int = 
        this.fold(_ => 1)((v,l,r) => l+r+v)
    def depth_fold:Int =
        this.fold(_=>0)((_,l,r) => l+r+1)
    def map_fold[B](f: A => B) : Tree[B] = 
        this.fold(v => Leaf(f(v)))((v,l,r) => v match 
            case Leaf(x) => Branch(x,l,r))

def maximum(t: Tree[Int]): Int = t match
    case Tree.Leaf(v)=>v
    case Tree.Branch(v,l,r) => v.max(maximum(l).max(maximum(r)))

def maximum_fold(t: Tree[Int]): Int = 
    t.fold(x => x)((v,l,r)=> v.max(l.max(r)))

case class ST[S,A](run: S=>(A,S)):
    def flatMap[B](f: A=> ST[S,B]): ST[S,B]=
        ST((s:S) => {val (a, ns) = run(s); f(a).run(ns)})
    def map[B](f: A=> B): ST[S,B]=
        ST((s:S) => {val (a, ns) = run(s); (f(a) , ns)})

object ST:
    def apply[S,A](a: => A) = new ST[S,A]((s:S)=>(a,s))

trait RNG:
    def nextInt: (Int, RNG)

    case class SimpleRNG(seed: Long) extends RNG:
        def nextInt: (Int, RNG) =
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            val nextRNG = SimpleRNG(newSeed)
            val n = (newSeed >>> 16).toInt
            (n, nextRNG)

trait Monad[T[_]]:
    def unit[F](x:F):T[F]
    extension [A](x: T[A])
        def flatMap[B](f: A => T[B]): T[B]

def RandomList(rng: RNG, n:Int): (List[Int], RNG) = 
    if  n==0  then (Nil, rng)
    val (x, r1) = rng.nextInt
    (x::RandomList(r1, n-1)._1,RandomList(r1, n-1)._2)

type Rand[A] = ST[RNG,A]

def nextInt: Rand[Int] = ST((rng:RNG) => rng.nextInt)

def nextDouble  : Rand[Double] = nextInt.map(x => x / (Int.MaxValue.toDouble+1))

def nextDouble_for : Rand[Double] = 
    for 
    x <- nextInt
    yield x / (Int.MaxValue.toDouble+1)

def intDouble : Rand[(Int,Double)] = 
    for 
    x <- nextInt
    y <- nextDouble
    yield (x,y)

def ints(count: Int): Rand[List[Int]] = 
    for 
    x <- nextInt
    y <- ints(count-1)
    yield x::y

type IntState[A] = ST[Int,A]

given IntState: Monad[IntState] with
    def unit[A](a: A): IntState[A] = ST((s:Int) => (a,s))
    extension  [A](m: IntState[A])
        def flatMap[B](f: A=> IntState[B]): IntState[B]=
            m.flatMap(f)
        def map[B](f: A=> B): IntState[B]=
            m.map(f)

    
    
    

