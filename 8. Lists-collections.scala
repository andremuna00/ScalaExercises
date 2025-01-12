
// One-liners, to warm up

def head[A] : List[A] => A = 
	case Nil => sys.error("empty list")
	case x::xs => x

def length[A] : List[A] => Int = 
    case Nil => 0
    case _::xs => 1 + length(xs)

def snoc[A](x:A, xs: List[A]) : List[A] = 
    xs match 
        case Nil => List(x)
        case (y::xs) => y::snoc(x,xs)

def reverse[A] : List[A] => List[A] = 
    case Nil => Nil
    case x::xs => snoc(x,reverse(xs))

def sumPairs : List[(Int,Int)] => Int = 
    case Nil => 0
    case (x,y)::xs => x+y + sumPairs(xs)

// Mutual recursion: define a function that selects the elements of the list that occur
// at odd positions in the list 

def odds[A](xs: List[A]) : List[A] = 
    def pick[A] : List[A] => List[A] = 
        case Nil => Nil
        case x::xs => x::drop(xs)
    def drop[A] : List[A] => List[A] = 
        case Nil => Nil
        case x::xs => pick(xs)
    pick(xs)


// Merge sort 
def split[A] : List[A] => (List[A], List[A]) = 
    case Nil => (Nil,Nil)
    case List(x) => (List(x), Nil)
    case x::y::xs => { val (xsl,xsr) = split(xs); (x::xsl, y::xsr) }

def msort[A](lt : (A,A) => Boolean, xs: List[A]) :  List[A] =
    def merge : (List[A],List[A]) => List[A] = 
        case (Nil, xs) => xs
        case (xs, Nil) => xs
        case (xs@x::xs1, ys@y::ys1) => 
            if lt(x,y) then x::merge(xs1,ys) else y::merge(xs,ys1)
    if (length(xs) <=1) then xs
    else 
        val (ys,zs) = split(xs)
        merge(msort(lt,ys), msort(lt,zs))


/* Given a list of chars, define a function words to return the corresponding list of 
   words, where a word is a list of chars other than the empty space 
*/
def words(xs: List[Char]) : List[List[Char]] = 
    // use an auxiliary function walk with returns the words of the list w:::xs, 
    // where xs is the (prefix of) the word under consideration
    def walk(w: List[Char], xs: List[Char]) : List[List[Char]] = 
      xs match 
         case Nil => List(w)                   // there is just w in xs
         case ' '::xs => w::walk(Nil,xs)       // word w found, start over
         case x::xs => walk(w:::List(x), xs)   // word w not completed: add x and continue
    walk(Nil, xs)

// works well 
// words(List('h','e','r','e',' ','i','s',' ','a','n',' ','e','x','a','m','p','l','e',' '))

// Uhm, not quite 
// words(List('a', ' '))
// words(List('a','b',' ',' ','c','d'))

// Problem shows up when w is empty. We must therefore treat this corner 
// so as not to trace empty words in our result.  The first two cases of the 
// new definition of walk handle this case, by just disregarding empty w's
// The remaining cases are as before 

def words_0(xs: List[Char]) : List[List[Char]] = 
    def walk(w: List[Char], xs: List[Char]) : List[List[Char]] = 
      (w,xs) match 
         case (Nil, Nil) => Nil
         case (Nil, ' '::xs) => walk(Nil, xs)
         case (w, Nil) => List(w)
         case (w, ' '::xs) => w :: walk(Nil, xs)
         case (w, x::xs)  => walk(w:::List(x), xs)
    walk(Nil, xs)

// Now everything looks ok
// words_0(List('a', ' '))
// words_0(List('a','b',' ',' ','c','d'))
// words_0(List('h','e','r','e',' ','i','s',' ','a','n',' ','e','x','a','m','p','l','e',' '))

// Higher Order 

def map[A,B](xs : List[A], f: A => B) : List[B]  = 
    xs match 
     case Nil => Nil
     case x::xs => f(x) :: map(xs, f) 

def filter[A](xs: List[A], p: A => Boolean) : List[A] = 
	xs match 
	case Nil => Nil
	case x::xs => if p(x) then x::filter(xs,p) else filter(xs,p)

// val ints  = List(1,2,3,4,5)
// val words = List("a", "list", "of", "words")

// map(ints, (x => x+1))      = List(2,3,4,5,6)
// map(words,(x => x.length)) = List(1,4,2,5) 
// map(words,(x => x.toList)) = List(List('a'), List('l','i','s','t'), List('o','f'),List('w','o',''r','d','s')


def foldRight[A, B](as: List[A], b: B, f: (A, B) => B): B = 
	as match
	case Nil => b
	case a::as => f(a, foldRight(as, b, f))

def lengthViaFoldRight[A](as: List[A]): Int = 
	foldRight(as, 0, (_,y) => y+1)

def appendViaFoldRight[A](as: List[A], bs: List[A]) : List[A] = 
	foldRight(as, bs, (x,y) => x::y) 

def reverseViaFoldRight[A](as: List[A]): List[A] = 
 	foldRight(as, List[A](), ((x,y) => y:+x))

def filterViaFoldRight[A](as: List[A], p: A => Boolean) = 
   foldRight(as, Nil, (x:A,y:List[A]) => if (p(x)) then y else x::y)

def mapViaFoldRight[A,B](as : List[A], f: A => B) : List[B]  = 
	foldRight(as, Nil, (x:A,y:List[B]) => f(x)::y) 

def pipe[A](fs : List[A=>A]) : A => A = 
    foldRight(fs, (x:A)=>x, (f,g) => (a => (f(g(a)))))

// FoldLeft

def foldLeft[A, B](as: List[A], b: B, f: (B, A) => B): B = 
	as match
	case Nil => b
	case a::as => foldLeft(as, f(b,a), f)

def reverseViaFoldLeft[A](as: List[A]): List[A] = 
	foldLeft(as, List[A](), (x,y) =>  y::x) 

def lengthViaFoldLcleaeft[A](as: List[A]): Int = 
	foldLeft(as, 0, (y,_) => y+1)


// Cascaded operations on collections

case class Invoice(client: String, amount: Double):
    def combine(other: Invoice): Invoice =
      require(client == other.client)
      Invoice(client, amount + other.amount)
      
// given a list of invoices return a list where each client is charged a single invoice with the 
// sum of all the amounts of his/her invoices in the original list
def coalesce(invoices: List[Invoice]): List[Invoice] =
    invoices.groupBy(inv=>inv.client).values.toList.map(invs=>invs.reduce((inv1,inv2) => inv1.combine(inv2)))


