/* Useful library datatypes */
import scala.collection.immutable.Set

/*------------------------------------------------------------------------------
   Abstract syntax of Lambda Terms 
  ------------------------------------------------------------------------------
*/

// Type aliases 
type VarName = String
type OpName = Char

// Scala implementation of the variant type representing lambda terms  
sealed trait LamTerm 

case class Var(name: VarName) extends LamTerm
case class Lam (name: VarName, body: LamTerm) extends LamTerm
case class App (fun: LamTerm, arg: LamTerm) extends LamTerm

case class IntLit(value: Int ) extends LamTerm
case class BoolLit(value: Boolean) extends LamTerm
case class BinOp(op: OpName, arg1: LamTerm, arg2 : LamTerm) extends LamTerm
case class UnOp(op: OpName, arg: LamTerm) extends LamTerm
case class Cond(guard: LamTerm, tbrach: LamTerm, fbranch: LamTerm) extends LamTerm
case class Fix(body: LamTerm) extends LamTerm


// Representation example: \x.\y.x 
val churchTrue = Lam("x", Lam("y",Var("x")))  

// Representation example: ((\x. (x x)) (\x. (x x)))
val omega =                               
    val lamXappXX = Lam("x",App(Var("x"),Var("x")))
    App(lamXappXX, lamXappXX)

            
/*------------------------------------------------------------------------------
   Free variables  
  ------------------------------------------------------------------------------
*/

def fv : LamTerm => Set[VarName] =  
    case Var(x)   => Set(x)
    case Lam(x,m) => fv(m) - x
    case App(m,n) => fv(m) ++ fv(n)
    case Cond(m,n,p) => fv(m) ++ fv(n) ++ fv(p)
    case BinOp(_,m,n) => fv(m) ++ fv(n)
    case UnOp(_,m) => fv(m)
    case Fix(m) => fv(m)
    case _ => Set()

/*------------------------------------------------------------------------------
   Substitution
  ------------------------------------------------------------------------------
*/

// Var companion object: a helper, imperative object to generate new var names, fresh in a given set

object Var: 
  private val baseName = "_"
  private var count = 0
  def fresh(names: Set[VarName]) : Var = 
   while (names contains baseName+count)
   do count +=1  
   new Var(baseName+count) 

// The substitution function: [N/x]M 
def subst(x: VarName, n: LamTerm, m: LamTerm) : LamTerm = 
  m match
    case Var(y)   => if (x == y) n else m
    case App(p,q) => App(subst(x,n,p),subst(x,n,q))
    case Lam(y,p) => if (x == y) m 
                     else if (!(fv(n) contains y)) Lam(y, subst(x,n,p))
                     else 
                        val Var(z) = Var.fresh(fv(n)++fv(p)+y)
                        Lam(z, subst(x,n,subst(y,Var(z),p)))

    case Cond(p,q,r)  => Cond(subst(x,n,p), subst(x,n,q), subst(x,n,r))
    case BinOp(o,p,q) => BinOp(o, subst(x,n,p), subst(x,n,q))
    case UnOp(o,m)    => UnOp(o, subst(x,n,m))
    case Fix(m)       => Fix(subst(x,n,m))

    case _ => m
 
// Example:  [y/x]((\y.x)(\x.x)x)

val term : LamTerm = App(App(Lam("y", Var("x")),Lam("x",Var("x"))),Var("x"))
val test = subst("x",Var("y"),term)


/*------------------------------------------------------------------------------
   Top-level reduction  
  ------------------------------------------------------------------------------
*/

 def reduce : LamTerm => LamTerm =  
    case App(Lam(x,m),n) => subst(x,n,m) 

    case BinOp('+', IntLit(n1), IntLit(n2)) => IntLit(n1+n2)
    case BinOp('-', IntLit(n1), IntLit(n2)) => IntLit(n1-n2)
    case BinOp('*', IntLit(n1), IntLit(n2)) => IntLit(n1*n2)
    case BinOp('/', IntLit(n1), IntLit(n2)) => IntLit(n1/n2)    
    case BinOp('&', BoolLit(b1), BoolLit(b2)) => BoolLit(b1&&b2)  
    case BinOp('|', BoolLit(b1), BoolLit(b2)) => BoolLit(b1||b2) 

    case UnOp('-', IntLit(n))  => IntLit(-n)
    case UnOp('!', BoolLit(b)) => BoolLit(!b)

    case (Cond(BoolLit(b), m, n)) => if (b) m else n 

    case (Fix(m)) => App(m,Fix(m))

    case m => m  // no reduction available


