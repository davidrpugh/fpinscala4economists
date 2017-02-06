sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  /** Exercise 3.2 */
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new NoSuchElementException("Cannot call tail on a Nil list!")
    case Cons(_, t) => t
  }

  /** Exercise 3.3 */
  def setHead[A](xs: List[A], x: A): List[A] = xs match {
    case Nil => throw new NoSuchElementException("Cannot set the head of a Nil list!")
    case Cons(_, t) => Cons(x, t)
  }

  /** Exercise 3.4 */
  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => if (n == 0) xs else drop(t, n - 1)
  }

  /** Exercise 3.5 */
  def dropWhile[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(h, t) if p(h) => dropWhile(t)(p)
    case _ => xs
  }

  /** Exercise 3.6 */
  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new NoSuchElementException("Cannot call init on a Nil list!")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /** Exercise 3.8 */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, count) => 1 + count)
  }

  /** Exercise 3.10 */
  def sum(ints: List[Int]): Int = {
    foldLeft(ints, 0)((total, x) => total + x)
  }

  /** Exercise 3.10 */
  def product(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)((prod, x) => prod * x)
  }

  /** Exercise 3.10 */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /** Exercise 3.11 */
  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((count, _) => count + 1)
  }

  /** Exercise 3.12 */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((t, h) => Cons(h, t))
  }

  /** Exercise 3.13 */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  /** Exercise 3.14 */
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((a, as) => Cons(a, as))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


/** Exercise 3.1:
  *
  * The following piece of code returns three.
  */
val l = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

/** Confirming that multiple parameter lists aid type inference! */
val xs: List[Int] = List(1,2,3,4,5)
val ex1 = List.dropWhile(xs)(x => x < 4)
List.reverse(xs)

List.sum(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

List.append(xs, xs)