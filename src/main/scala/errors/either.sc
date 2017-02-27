sealed trait Either[+E, +A] {

  /** Exercise 4.6 */
  def map[B](f: A => B): Either[E, B] = this match {
    case left @ Left(_) => left
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case left @ Left(_) => left
    case Right(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case right @ Right(_) => right
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this flatMap (v1 => b map (v2 => f(v1, v2)))
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


object Either {

  /** Exercise 4.7 */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(a => a)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: tail => f(h).flatMap(a => traverse(tail)(f).map(bs =>  a :: bs ))
  }

}

def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
  if (xs.isEmpty) Left("mean of empty list!") else Right(xs.sum / xs.length)
}
