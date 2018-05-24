package functors


trait Applicative[F[_]] extends Functor[F] {

  // Exercise 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fab, fa)((g, a) => g(a))
  }

  // Exercise 12.9
  def compose[G[_]](ap: Applicative[G]): Applicative[({ type f[A] = F[G[A]] })#f] = {
    val self = this
    new Applicative[({ type f[A] = F[G[A]] })#f] {
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        self.map2(fga, fgb)((ga, gb) => ap.map2(ga, gb)(f))
      }
      def unit[A](a: => A): F[G[A]] = {
        self.unit(ap.unit(a))
      }
    }
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    map2(fa, unit(()))((a, _) => f(a))
  }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    // apply(map(fa)(f.curried))(fb)
    map(product(fa, fb))(f.tupled)
  }

  // Exercise 12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(map2(fa, fb)((a, b) => f.curried(a)(b)))(fc)
  }

  // Exercise 12.3
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(map3(fa, fb, fc)((a, b, c) => f.curried(a)(b)(c)))(fd)
  }

  // Exercise 12.1
  def filterM[A](as: List[A])(f: (A) => F[Boolean]): F[List[A]] = {
    as.foldRight(unit(List.empty[A]))((a, fas) => map2(f(a), fas)((b, as) => if (b) a :: as else as))
  }

  // Exercise 12.1
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    map2(fa, fb)((a, b) => (a, b))
  }

  def product[G[_]](ap: Applicative[G]): Applicative[({ type f[A] = (F[A], G[A]) })#f] = {
    val self = this
    new Applicative[({ type f[A] = (F[A], G[A]) })#f] {
      override def apply[A, B](aps: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) = {
        (self.apply(aps._1)(p._1), ap.apply(aps._2)(p._2))
      }
      def unit[A](a: => A): (F[A], G[A]) = {
        (self.unit(a), ap.unit(a))
      }
    }
  }

  // Exercise 12.1
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    @annotation.tailrec
    def loop(i: Int, fas: F[List[A]]): F[List[A]] = {
      if (i <= 0) {
        fas
      } else {
        loop(i - 1, map2(fa, fas)((a, as) => a :: as))
      }
    }
    loop(n, unit(List.empty[A]))
  }

  // Exercise 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    fas.foldRight(unit(List.empty[A]))((fa, fas) => map2(fa, fas)((a, as) => a :: as))
  }

  // Exercise 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldLeft(unit(Map.empty[K, V])){ case (fm, (k, fv)) => map2(fm, fv)((m, v) => m + (k -> v)) }
  }


  // Exercise 12.1
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))
  }

  def unit[A](a: => A): F[A]

}


object Applicative {

  val stream: Applicative[Stream] = new Applicative[Stream] {
    override def map2[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] = {
      as.zip(bs).map(f.tupled)
    }
    def unit[A](a: => A): Stream[A] = {
      Stream.continually(a)
    }
  }

}
