abstract class QueueTest:
    def get(): Int
    def put(x: Int): Unit

class BasicQueue extends QueueTest:
    var elems: List[Int] = Nil
    def get() = elems.head
    def put(x: Int) = elems = elems :+ x

trait Doubling extends QueueTest:
    abstract override def put(x: Int) = super.put(2 * x)

trait Incrementing extends QueueTest:
    abstract override def put(x: Int) = super.put(x + 1)

trait Filtering extends QueueTest:
    def filter(x: Int): Boolean
    abstract override def put(x: Int) = if (filter(x)) super.put(x)

val q = new BasicQueue with Incrementing with Filtering:
    def filter(x: Int) = x >= 0

q.put(-1); q.put(0); q.put(1)
q.get() // 1

//DEFINITION PURELY FUNCTIONAL
//no+A perchè se visualizziamo la classe come un record di funzioni, non è possibile che la funzione/campo enqueue sia una funzione che covaria nel tipo di input 
class Queue[A](private val leading: List[A], private val trailing: List[A]):
    def this() = this(Nil, Nil)
    def reverse : Queue[A] = 
        if leading.isEmpty then new Queue(trailing.reverse, Nil)
        else this
    def head : A = 
        reverse.leading.head
    def tail : Queue[A] = {
        val q = reverse
        new Queue(q.leading.tail, q.trailing)
    }
    def enqueue(x: A) : Queue[A]= 
        new Queue(leading, x :: trailing)
    def dequeue : Queue[A] = 
        val q = reverse
        new Queue(q.leading.tail, q.trailing)

object Queue:
    def apply[A](xs: A*) = new Queue[A](xs.toList, Nil)

val f = Queue(1,2,3)


class QueueGeneric[+A](private val leading: List[A], private val trailing: List[A]):
    def this() = this(Nil, Nil)
    def reverse : QueueGeneric[A] = 
        if leading.isEmpty then new QueueGeneric[A](trailing.reverse, Nil)
        else this
    def head : A = 
        reverse.leading.head
    def tail : QueueGeneric[A] = {
        val q = reverse
        new QueueGeneric[A](q.leading.tail, q.trailing)
    }
    def enqueue[B >: A](x: B) : QueueGeneric[B]= 
        new QueueGeneric[B](leading, x :: trailing)
    def dequeue : QueueGeneric[A] = 
        val q = reverse
        new QueueGeneric(q.leading.tail, q.trailing)


trait Stream[+A]:
    //We can use A and not B>:A because A is not the input type of filter but the input type of the parameter input of the function filter (so double negative -> positive)
    def filter(p: A => Boolean): Stream[A]
    def map[B](f: A => B): Stream[B]


//upper and lower bounds
trait JavaStream[+T]:
    def filter[U>:T](p: T => Boolean): JavaStream[T]
    def map[R, U>:T, S<:R](f: U => S): JavaStream[R]