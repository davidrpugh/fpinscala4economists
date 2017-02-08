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
def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
  case Nil => Some(Nil)
  case h :: tail => h.flatMap(a => sequence(tail).map(bs =>  a :: bs ))
}

/** Exercise 4.5 */
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case h :: tail => f(h).flatMap( b => traverse(tail)(f).map(bs => b :: bs))
}

def sequence2[A](as: List[Option[A]]): Option[List[A]] = {
  traverse(as)(a => a)
}


// quick test of my traverse implementation
val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
val evens = xs.filter(_ % 2 == 0)
assert(traverse(xs)(x => if (x % 2 ==0) Some(x) else None) == None)
assert(traverse(evens)(x => if (x % 2 ==0) Some(x) else None) == Some(evens))