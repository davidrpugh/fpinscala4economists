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
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b, a))
  }

  /** Exercise 3.14 */
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((a, as) => Cons(a, as))
  }

  /** Exercise 3.15 */
  def concat[A](lists: List[List[A]]): List[A] = {
    foldLeft(lists, Nil: List[A])(append)
  }

  /** Exercise 3.16 */
  def increment(xs: List[Int]): List[Int] = {
    foldLeft(xs, Nil: List[Int])((t, h) => Cons(h+1, t))
  }

  /** Exercise 3.17 */
  def doublesToStrings(xs: List[Double]): List[String] = {
    foldRight(xs, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  /** Exercise 3.18 */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  /** Exercise 3.19 */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  /** Exercise 3.20 */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((a, l) => append(f(a), l))
  }

  /** Exercise 3.21 */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)
  }

  /** Exercise 3.22 */
  def pairwiseadd(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, pairwiseadd(t1, t2))
  }

  /** Exercise 3.23 */
  def zipWith[A, B, C](as: List[A], bs: List[B])(f:(A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(f(a, b), zipWith(ta, tb)(f))
  }

  /** Exercise 3.24 */
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](l: List[A], subSequence: List[A]): Boolean = l match {
    case Nil => subSequence == Nil
    case _ if startsWith(l, subSequence) => true
    case Cons(h, t) => hasSubsequence(t, subSequence)
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

List.concat(List(xs, xs, xs))
List.sum(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

List.append(xs, xs)


// test of the filter method
val ints = List(1,2,3,4,5,6,7,8,9,10)
val odds = List.filter(ints)(x => x % 2 == 1)
assert(odds == List(1,3,5,7,9))

// test of the filter2 method
val evens = List.filter2(ints)(x => x % 2 == 0)
assert(evens == List(2,4,6,8,10))

// test of the flatMap method
assert(List.flatMap(List(1,2,3))(i => List(i-1,i)) == List(0,1,1,2,2,3))