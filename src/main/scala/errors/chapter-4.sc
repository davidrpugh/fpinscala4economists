sealed trait Option[+A] {

  /** Exercise 4.1 */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map(value => Some(value)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None else Some(xs.sum / xs.length)
}

/** Exercise 4.2 */
def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}


// simple test for the variance implementation
assert(variance(Seq.fill(5)(10)) == Some(0.0))


def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

/** Exercise 4.3 */
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  a flatMap (v1 => b map (v2 => f(v1, v2)))
}

/** Exercise 4.4 */
def sequence[A](a: List[Option[A]]): Option[List[A]]