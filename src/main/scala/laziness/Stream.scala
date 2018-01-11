package laziness

import scala.collection.mutable

sealed trait Stream[+A] {

  /** Exercise 5.7 */
  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a, b) => Stream.cons(a, b))
  }

  /** Exercise 5.2 */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists2(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  /** Exercise 5.7 */
  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)
  }

  def find(p: A => Boolean): Option[A] = {
    filter(p).headOption
  }

  /** Exercise 5.7 */
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => f(h).append(t))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  /** Exercise 5.4 */
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def hasSubsequence[A1 >: A](s: Stream[A1]): Boolean =
    tails exists (_ startsWith s)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /** Exercise 5.6 */
  def headOption2: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  /** Exercise 5.7 */
  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))
  }

  /** Exercise 5.13 */
  def map2[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  /** Exercise 5.14 */
  def startsWith[A1 >: A](s: Stream[A1]): Boolean = {
    zipAll(s).takeWhile{ case (_, opt2) => opt2.nonEmpty }.forAll{ case (a1, a2) => a1 == a2 }
  }

  /** Exercise 5.15 */
  def tails: Stream[Stream[A]] = {
    val result = Stream.unfold(this){
      case s @ Cons(h, t) => Some(s, t())
      case Empty => None
    }
    result.append(Stream(Stream.empty))
  }

  /** Exercise 5.2 */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  /** Exercise 5.13 */
  def take2(n: Int): Stream[A] = Stream.unfold((this, n)) {  // note the clever use of state!
    case (Cons(h, _), 1) => Some(h(), (Stream.empty, 0))
    case (Cons(h, t), k) => Some(h(), (t(), k - 1))
    case _ => None
  }

  /** Exercise 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  /** Exercise 5.5 */
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)
  }

  /** Exercise 5.13 */
  def takeWhile3(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  /** Exercise 5.1 */
  def toList: List[A] = {

    // mutable data-structure never escapes the function!
    val buf = mutable.ListBuffer[A]()

    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) => buf += h(); go(t())  // SIDE EFFECT!
    }

    go(this)
  }

  /** Exercise 5.13 */
  def zip[B](s2: Stream[B]): Stream[(A, B)] = {
    zipWith(s2)((a, b) => (a, b))
  }

  /** Exercise 5.13 */
  def zipWith[B, C](s2: Stream[B])(f:(A, B) => C): Stream[C] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  /** Exercise 5.13 */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    zipWithAll(s2)((_, _))
  }

  /** Exercise 5.13 */
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = Stream.unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]), (t(), Stream.empty[B]))
    case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())), (Stream.empty[A], t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd; lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  /** Exercise 5.8 */
  def constant[A](elem: A): Stream[A] = {
    unfold(elem)(_ => Some(elem, elem))
  }

  /** Exercise 5.9 */
  def from(n: Int): Stream[Int] = {
    unfold(n)(p => Some(p, p + 1))
  }

  /** Exercise 5.11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

}
