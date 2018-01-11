import laziness.Stream
import state.RNG
import testing._







def listOf[A](g: Gen[A]): Gen[List[A]] = ???

val intList = listOf(Gen.choose(0,100))
val prop = {
  forAll(intList)(ns => ns.reverse.reverse == ns) && forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
}