package monads

import functors.Applicative
import parallelism.Par
import state.State
import testing.Gen


trait Monad[F[_]] extends Applicative[F] {

  def compose[A, B, C](f: (A) => F[B], g: (B) => F[C]): (A) => F[C] = {
    a => flatMap(f(a))(g)
  }

  def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B]

  def _flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B] = {
    compose((_: Unit) => fa, f)(())
  }

  def join[A](ffa: F[F[A]]): F[A] = {
    flatMap(ffa)(identity)
  }

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] = {
    flatMap(fa)(a => unit(f(a)))
  }

  def unit[A](a: => A): F[A]

}


object Monad {

  val genMonad = new Monad[Gen] {
    def flatMap[A, B](g: Gen[A])(f: (A) => Gen[B]): Gen[B] = {
      g.flatMap(f)
    }
    def unit[A](a: => A): Gen[A] = {
      Gen.unit(a)
    }
  }

  type Identity[A] = A

  val identity = new Monad[Identity] {
    def flatMap[A, B](a: Identity[A])(f: (A) => Identity[B]): Identity[B] = {
      f(a)
    }
    def unit[A](a: => A): Identity[A] = {
      a
    }
  }

  val listMonad = new Monad[List] {
    def flatMap[A, B](as: List[A])(f: (A) => List[B]): List[B] = {
      as.flatMap(f)
    }
    def unit[A](a: => A): List[A] = {
      List(a)
    }
  }

  val parMonad = new Monad[Par] {
    def flatMap[A, B](p: Par[A])(f: (A) => Par[B]): Par[B] = {
      Par.flatMap(p)(f)
    }
    def unit[A](a: => A): Par[A] = {
      Par.unit(a)
    }
  }

  val optionMonad = new Monad[Option] {
    def flatMap[A, B](o: Option[A])(f: (A) => Option[B]): Option[B] = {
      o.flatMap(f)
    }
    def unit[A](a: => A): Option[A] = {
      Option(a)
    }
  }

  val streamMonad = new Monad[Stream] {
    def flatMap[A, B](as: Stream[A])(f: (A) => Stream[B]): Stream[B] = {
      as.flatMap(f)
    }
    def unit[A](a: => A): Stream[A] = {
      Stream(a)
    }
  }

  def stateMonad[S] = new Monad[({ type f[A] = State[S, A] })#f] {
    def flatMap[A, B](s: State[S, A])(f: (A) => State[S, B]): State[S, B] = {
      s.flatMap(f)
    }
    def unit[A](a: => A): State[S, A] = {
      State(s => (a, s))
    }
  }

  // Exercise 12.5
  def eitherMonad[L] = new Monad[({ type f[R] = Either[L, R] })#f] {
    def flatMap[R1, R2](e: Either[L, R1])(f: R1 => Either[L, R2]): Either[L, R2] = {
      e.flatMap(f)
    }
    def unit[R](r: => R): Either[L, R] = {
      Right(r)
    }
  }

}
