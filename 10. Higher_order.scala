/* =========================================================================================================
    Function literals and type inference  
   ========================================================================================================= 
*/
def foldRight[A, B](as: List[A], b: B, f: (A, B) => B): B = 
	as match
	case Nil => b
	case a::as => f(a, foldRight(as, b, f))

def  append[A](as: List[A], bs: List[A]) : List[A] =
		foldRight(as, bs, (x,y) => x::y)

def  reverse[A](as: List[A]): List[A] =
        foldRight(as, List[A](), ((x,y) => y:+x))

def  map[A,B](f: A => B)(as: List[A]): List[B] = 
        foldRight(as, List[B](), (x,y) => f(x)::y)

def  filter[A](p: A => Boolean)(as: List[A])  =
        foldRight(as, Nil, (x,y:List[A]) => if (p(x)) then y else x::y)


/* =========================================================================================================
    Closures, scope and mutability     
   ========================================================================================================= 
*/

// Closures 
def addx(x: Int) = 
	(y: Int) => x + y

def showScope = 
    val x : Int = 3
    def f (y : Int) = x + y
    def show = 
        val x : Int = 4
        f(3) + x
    show

var more = 1
val addMore = (x: Int) => x + more
addMore(10)  // 11
more = 37
addMore(10) // 47

/* =========================================================================================================
    Currying      
   ========================================================================================================= 
*/

def curry[A,B,C](f: (A,B) => C) = (x:A) => (y:B) => f(x,y) 
def uncurry[A,B,C](f: A => B => C)  = (x:A,y:B) => f(x)(y)

def add (x:Int, y:Int) : Int = x+y   
def addc (x:Int) (y:Int) : Int  = x+y

/* =========================================================================================================
    Composition 
   ========================================================================================================= 
*/

extension[A,B](f: A=>B)
    def `>>` [C](g:B=>C) : A=>C = (a:A) => g(f(a))

def f : Int => Int = (_+10)
def g : Int => Int = (_*3)

def h = f >> g

/* =========================================================================================================
    Flipping 
   ========================================================================================================= 
*/

def flip[A,B,C] : (A => B => C) => B => A => C = 
	(f: A => B => C) => (b:B) => (a:A) => f(a)(b)

// Example
def scanleft[A,B](f: B => A => B)(z: B)(xs: List[A]) : List[B] = 
    xs match 
        case Nil => Nil
        case x::xs => z :: scanleft(f)(f(z)(x))(xs)

def snoc[A] = (a:A) => (as:List[A]) => as:+a 

def prefixes[A](as: List[A]): List[List[A]] = scanleft(flip(snoc))(List[A]())(as)


/* =========================================================================================================
    Quicksort  
   ========================================================================================================= 
*/

def quicksort : List[Int] => List[Int]  = 
	case Nil => Nil  // nothing to sort 
	case x :: xs =>  // use x as pivot
		quicksort(xs.filter(_<x)) ::: (x :: xs.filter(_==x)) ::: quicksort(xs.filter(_>x))


