package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil =>  sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] =
  if (n<=0) l
  else l match {
    case Nil => Nil
    case Cons(_, t) => drop(t, n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x,xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,acc)=> acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def sum3(l: List[Int]) = foldLeft(l,0)(_+_)
  def product3(l: List[Int]) = foldLeft(l,1)(_*_)
  def length3(l: List[Int]) = foldLeft(l,0)((acc,_) => acc+1)
  def reverse3[A](l: List[A]) = foldLeft(l, List[A]())((xs,x) => Cons(x,xs))
  def appendUsingFoldRight[A](a: List[A], b:List[A]) = foldRight(a, b)((x,xs)=> Cons(x,xs))
  def appendUsingFoldLeft[A](a: List[A], b:List[A]) = foldLeft(reverse3(a), b)((xs,x)=> Cons(x,xs))
  def add_1(as: List[Int]): List[Int] = reverse3(foldLeft(as, List[Int]())((xs,x) => Cons(x+1,xs)))
  def doubleToString(as: List[Double]): List[String] = foldRight(as, Nil:List[String])((x,xs) => Cons(x.toString, xs))
  def map3[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((x,xs)=> Cons(f(x), xs))
  def flatMap3[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil:List[B])((x,xs)=> appendUsingFoldRight(f(x), xs))
  def filter3[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((x,xs) => if(f(x)) Cons(x,xs) else xs )
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap3(as)(x => if(f(x)) List(x) else Nil)
  def addPairWise(xs: List[Int], ys: List[Int]): List[Int] = (xs,ys) match {
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (Cons(x,x1), Cons(y,y1)) => Cons(x+y, addPairWise(x1,y1))
  }
  def zip3[A](xs: List[A], ys: List[A])(f: (A,A) => A): List[A] = (xs,ys) match {
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (Cons(x,x1), Cons(y,y1)) => Cons(f(x,y), zip3(x1,y1)(f))
  }
}
