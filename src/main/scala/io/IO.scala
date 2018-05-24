package io

import monads.Monad


sealed trait IO[A] {

  def flatMap[B](f: A => IO[B]): IO[B] = {
    FlatMap(this, f)
  }

  def map[B](f: A => B): IO[B] = {
    flatMap(f.andThen(b => Return(b)))
  }

}

/* pure computation that returns an A without any further processing. */
case class Return[A](a: A) extends IO[A]
/* suspends the computation where resume is a function that takes no args but has some effect and yields a result. */
case class Suspend[A](resume: () => A) extends IO[A]
/* composition of two steps. When run sees this it should first process the sub-computation and then continue with f. */
case class FlatMap[A, B](sub: IO[A], f: A => IO[B]) extends IO[B]

object IO extends Monad[IO] {

  def apply[A](a: => A): IO[A] = {
    unit(a)
  }

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = {
    fa.flatMap(f)
  }

  def printLine(s: String): IO[Unit] = {
    Suspend(() => Return(println(s)))
  }

 @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()  // function r takes no args!
    case FlatMap(s0, f0) => s0 match {
      case Return(a) => run(f0(a))
      case Suspend(r) => run(f0(r()))
      case FlatMap(s1, f1) => run(s1.flatMap(a => f1(a).flatMap(f0)))
    }
  }

  def unit[A](a: => A): IO[A] = {
    Return(a)
  }

}
