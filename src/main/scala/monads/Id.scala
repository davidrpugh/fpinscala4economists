package monads


case class Id[A](value: A) {

  def flatMap[B](f: A => Id[B]): Id[B] = {
    f(value)
  }

  def map[B](f: (A) => B): Id[B] = {
    Id(f(value))
  }

}


object Id {

  implicit val idMonad: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = {
        fa.flatMap(f)
    }
    def unit[A](a: => A): Id[A] = {
      Id(a)
    }
  }

}
