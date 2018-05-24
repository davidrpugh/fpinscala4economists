package functors


trait Functor[F[_]] {
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(a => Left(a))
    case Right(fb) => map(fb)(b => Right(b))
  }
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = {
    (map(fab){ case (a, _) => a }, map(fab){ case (_, b) => b })
  }
  def map[A, B](fa: F[A])(f: (A) => B): F[B]
}


object Functor {

  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: (A) => B): List[B] = {
      as.map(f)
    }
  }

}
