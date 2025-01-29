trait Monad [F[_]]:
    def unit[A](x:A): F[A]
    extension[A](ma:F[A])
        def flatmap[B](f:A=>F[B]):F[B]
    extension
    


extension[A](as:List[A])
    def MytoString():String=as match
        case Nil=>"List()"
        case x::xs=>val l = xs.toString()
                    val c = l.substring(0,5)+x.toString()
                    if(l.substring(5)==")") then c+l.substring(5)
                    else c+","+l.substring(5)

def unzip[A,B](as:List[(A,B)]): (List[A],List[B])= as match
    case Nil=>(Nil,Nil)
    case (x,y)::xs=>(x::unzip(xs)._1, y::unzip(xs)._2)

def unzipR[A,B](as:List[(A,B)]): (List[A],List[B])=
    foldRight(as, (Nil,Nil), ((x, b:(List[A],List[B]))=>(x._1::b._1,x._2::b._2)))


trait MyStream[+A]
case object Empty extends MyStream[Nothing]
case class cons[A](hd: A, tl:()=>MyStream[A]) extends MyStream[A]

def unfold[A,S](state:S)(f:S=>Option[(A,S)]):MyStream[A]=
    f(state) match
        case Some((h,s))=>cons(h, ()=>unfold(s)(f))
        case None=>Empty

def take[A](as:MyStream[A],n:Int):MyStream[A]=
    unfold(n:Int,as)(y:(Int, MyStream[A])=>if y._1==0 || y._2==Empty then None else y._2 match
        case x::xs=>(x,(y._1,xs)))


given Monad[List] with
    def unit[A](a:A)=List(a)
    extension[A](ma:List[A])
        def flatmap[B](f:A=>List[B]):List[B]= ma match
            case Nil => Nil
            case x::xs => f(x):::xs.flatmap(f)
        
