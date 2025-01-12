/* A motivating example for variance. Taken from "Programming in Scala", Chapter 18 */

 class Queue[+A](private val leading: List[A], private val trailing: List[A]):

    def this() = this(Nil,Nil)

    def reverse : Queue[A] = 
        if leading.isEmpty then new Queue(trailing.reverse, Nil)
        else this
	
    def head : A = 
        reverse.leading.head 

    def dequeue : Queue[A] = 
        val q = reverse
        new Queue(q.leading.tail, q.trailing)

    def enqueue[B >:A](a: B) =
        new Queue[B](leading, a::trailing)

object Queue: 
    def apply[A](as: A*) = new Queue[A](as.toList, Nil)

// With the covariant definition this does not type check 
// class WeirdQueue extends Queue[Double]:
//    override def enqueue[B >: Double](a: B) = super.enqueue(math.pow(a,2))


// Contravariant types and upper bounds 
trait OutputChannel[-A]:
	def write(x: A) : Unit
	def last[B<:A] : B 

/* Scala variant types vs Java’s wildcards */

// Points and Lines 
class Point(val x:Int, val y:Int)
class ColorPoint(x:Int, y:Int, val c:Int) extends Point(x,y)
class Line(val m: Double, val q: Double) 

// Java Streams 
class JavaStream[T](val elements: List[T]):
	def filter[U >:T](p : U => Boolean): JavaStream[T] = new JavaStream[T](elements.filter(p)) 

object JavaStream: 
    def apply[A](as : A*) : JavaStream[A] = new JavaStream[A](as.toList)

def javaPointsOnLine[U <: Point](pts : JavaStream[U], l: Line) = 
    def belongs(p: Point) : Boolean = (p.y == l.m * p.x + l.q)
    pts.filter(belongs)

val javaColorpoints : JavaStream[ColorPoint] = JavaStream[ColorPoint]()
javaPointsOnLine(javaColorpoints, new Line(3,5))

// Scala Streams 
class ScalaStream[+T](val elements: List[T]):
	def filter(p : T => Boolean): ScalaStream[T] = new ScalaStream(elements.filter(p)) 

object ScalaStream: 
    def apply[A](as : A*) : ScalaStream[A] = new ScalaStream[A](as.toList)

def scalaPointsOnLine(pts : ScalaStream[Point], l: Line) = 
    def belongs(p: Point) : Boolean = (p.y == l.m * p.x + l.q)
    pts.filter(belongs)

val scalaColorpoints : ScalaStream[ColorPoint] = ScalaStream[ColorPoint]()
scalaPointsOnLine(scalaColorpoints, new Line(3,5))



