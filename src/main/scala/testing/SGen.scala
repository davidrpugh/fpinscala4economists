package testing


case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = {
    forSize(n)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))
  }

  def map[B](f: A => B): SGen[B] = {
    SGen(n => forSize(n).map(f))
  }

}


object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => Gen.listOfN(n, g))
  }

}