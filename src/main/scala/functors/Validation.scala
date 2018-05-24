package functors


sealed trait Validation[+E, +A]

object Validation {

  def applicative[E] = new Applicative[({ type f[A] = Validation[E, A] })#f] {
    override def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (va, vb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Success(_), failure @ Failure(h, t)) => failure
        case (failure @ Failure(h, t), Success(_)) => failure
        case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta ++ Vector(hb) ++ tb)
      }
    }
    def unit[A](a: => A): Validation[E, A] = {
      Success(a)
    }
  }

}

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]
