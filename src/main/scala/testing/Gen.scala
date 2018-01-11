package testing

import state.{RNG, State}


case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def unsized: SGen[A] = {
    SGen((_) => this)
  }

}


object Gen {

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def choose(start: Int, stop: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + (stop % n)))
  }

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(gen.sample)))
  }

  def pair(start: Int, stop: Int): Gen[(Int, Int)] = {
    ???
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def weighted[A](p1: (Gen[A],Double), p2: (Gen[A],Double)): Gen[A] = {
    val (g1, w1) = p1; val (g2, w2) = p2
    val threshold = w1 / (w1 + w2)
    Gen(State(RNG.double).flatMap(p => if (p <= threshold) g1.sample else g2.sample))
  }

}
