import Exercises.Monad
def cps_add[A](x:Int, y:Int): (Int=>A)=>A =
    (k:Int=>A) => k(x+y)

def flatMap[A,B](as: List[A], f: A=>List[B]): List[B] = 
    as match
        case Nil => Nil
        case x::xs => f(x):::flatMap(xs,f)

def foldRight[A,B](as: List[A], b: B, f:(A,B)=>B):B=
    as match
        case Nil=>b
        case x::xs => f(x, foldRight(xs, b, f))

def foldLeft[A,B](as: List[A], b: B, f:(B,A)=>B):B=
    as match
        case Nil=>b
        case x::xs => foldLeft(xs, f(b,x), f)

def swap[A,B,C](f:(A,B)=>C) = (y:B,x:A)=>f(x,y)

trait Stream[A]
case class cons[A](x: A, xs: ()=>Stream[A]) extends Stream[A]

def summon[T](using x: T) = x

def filter[A](x: Stream[A], p:(A=>Boolean)):Stream[A]=
    x match
        case cons(h,t) => if(p(h)) then cons(h, ()=>filter(t(),p)) else filter(t(),p)


def sequence[A](as: List[Option[A]]): Option[List[A]]= 
    as match
        case Nil => Some(Nil)
        case x::xs => sequence(as).flatMap(z=>x.map(y=>y::z))
    
extension [A](o: Option[A])
    def map2[B,C](ob: Option[B])(f:(A,B) => C) : Option[C] =
        o.flatMap(x => ob.map(y=>f(x,y)))

def sequence_fold[A](as: List[Option[A]]): Option[List[A]]=
    foldRight(as, Some(Nil), (a,b:Option[List[A]])=>a.map2(b)((x,y)=>x::y)) 

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match
        case Nil => Some(Nil)
        case x::xs => f(x).map2(traverse(xs)(f))(_::_)

def traverse_sequence[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(as.map(f))

trait Tree[A]
case class Node[A](left: Tree[A], right: Tree[A], value:A) extends Tree[A]
case class Leaf[A](value:A) extends Tree[A]

sealed trait Shape[T<:Shape[T]]:
  def area : Double
  def scale(f:Double): T

case class Circle(radius: Double) extends Shape[Circle]:
  def area: Double = Math.PI * Math.pow(radius,2)
  def scale(f:Double) : Circle = Circle(radius*f)

case class Rectangle(base: Double, height: Double) extends Shape[Rectangle]:
  def area: Double = base * height
  def scale(f:Double) : Rectangle = Rectangle(base*f, height*f)
        
