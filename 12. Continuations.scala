
/* =========================================================================================================
    CPS introduction - pythagoras   
   ========================================================================================================= 
*/

def report[A](res : A) : String = "Here's the result: " + res.toString 

def add(x: Int, y: Int) = x+y 
def square (x: Int) = x*x 

def cps_add[A](x: Int, y: Int) : (Int => A) => A = 
	(k: Int => A) => k (add(x,y))
def cps_square[A](x: Int) : (Int => A) => A = 
	(k: Int => A) => k (square(x))

def cps_pythagoras[A](x: Int, y: Int) : (Int => A) => A = 
	(k: Int => A) => cps_square(x) 
		((xsquare: Int) => cps_square(y) 
			((ysquare: Int) => cps_add (xsquare, ysquare)(k)))

/* =========================================================================================================
    CPS and tail recursion    
   ========================================================================================================= 
*/
def cps_fact[A](n: Int) : (Int => A) => A = 
	require(n >= 0)
	(k: Int => A) => 
		if (n == 0) then k(1)
		else cps_fact(n-1)((m:Int) => k(m*n)) 

def cps_append[A,B](as: List[A], bs: List[A]) :  (List[A] => B) => B = 
		(k: List[A] => B) => as match 
			case Nil => k(bs)
			case a::as => cps_append(as,bs)((abs:List[A]) => k(a::abs)) 

/* =========================================================================================================
    CPS and local exits    
   ========================================================================================================= 
*/
enum Tree: 
    case Node(left: Tree, right: Tree) extends Tree
    case Leaf(value: Int)

import Tree.*

def product[A](t: Tree) : Int = 
    t match 
        case Leaf(n) => n
        case Node(l,r) => product(l) * product(r)


def cps_product[A](t: Tree)(k : Int => A) : A =
    def prod(t: Tree)(h: Int => A) : A =  
        t match 
            case Leaf(n) => 
                // immediate exit upon hitting a zero leaf: uncomment the following line to trace the effect
                println(n)
                if (n == 0) then k(0) else h(n)
            case Node(l,r) => 
                prod(l)((rleft: Int) => 
                    prod(r)((rright: Int) => h(rleft * rright)))
    prod(t)(k)


def non_neg[A](t: Tree) : Boolean = 
    t match 
        case Leaf(n) => (n < 0)
        case Node(l,r) => non_neg(l) && non_neg(r)


def cps_non_neg[A](t: Tree)(k : Boolean => A) : A =
    def check(t: Tree)(h: Boolean => A) : A =  
        t match 
            case Leaf(n) => 
                // immediate exit upon hitting a negative leaf: uncomment the following line to trace the effect
                // println(n)
                if (n < 0) then k(false) else h(true)
            case Node(l,r) => 
                check(l)((rleft: Boolean) => 
                    check(r)((rright: Boolean) => h(rleft & rright)))
    check(t)(k)

val tree  = Node(Node(Leaf(1),Leaf(-2)), Node(Leaf(8),Leaf(5)))
val tree0  = Node(Node(Leaf(1),Leaf(0)), Node(Leaf(8),Leaf(5)))


/* =========================================================================================================
    CPS and backtracking     
   ========================================================================================================= 
*/
    
enum BF:
    case Var(name : String)
    case Not(bf: BF)
    case And(bfl: BF, bfr: BF)
    case Or(bfl: BF, bfr: BF)

type Asst = Map[String, Boolean]

import BF.* 

def sat[A](b : BF, 
        asst: Asst, 
        succ: (Boolean, Asst, () => A) => A, 
        fail: () => A) : A = 
    b match 
        case Var(a) => 
            asst.get(a) match
                case Some(b) => 
                    succ(b, asst, fail)
                case None => 
                    val tryF = () => succ(false, asst + (a -> false), fail)
                    val tryT = succ(true, asst + (a -> true), tryF)
                    tryT

        case Not(bf) =>
            sat(bf, asst, (b,asst,fail) => succ(!b,asst,fail), fail)

        case And(bfl, bfr) => 
            sat(bfl, 
                asst,
                (b, asst, f) => if b then sat(bfr,asst,succ,f) else succ(false, asst, f), 
                fail)

        case Or(bfl,bfr) => 
            sat(bfl, 
                asst,
                (b,asst,f) => if b then succ(true, asst, f) else sat(bfr, asst, succ, f), 
                fail)

def satisfy(bf : BF) : Option[Asst] = 
    sat(bf, 
        Map.empty, 
        (b, asst, fail) => if b then Some(asst) else fail(),
        () => None)



val test1 = And(Var("a"),And(Not(Var("b")), Var("c")))
val test2 = And(Var("a"), Not(Var("a")))
val test3 = Or(Var("a"), And(Not(Var("b")), Var("c")))
val test4 = And(Or(Var("a"), Or(Var("b"), Var("c"))), And(Not(Var("a")), Not(Var("b"))))

// satisfy(test1) should be Some(Map(a -> true, b -> false, c -> true))
// satisfy(test2) should be None
// satisfy(test3) should be Some(Map(a -> true))
// satisfy(test4) should be Some(Map(a -> false, b -> false, c -> true))


