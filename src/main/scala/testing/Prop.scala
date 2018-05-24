package testing

import laziness.Stream
import state.{LinearCongruentialGenerator, RNG}


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def && (p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def || (p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {

  def buildMsg[A](s: A, e: Exception): String = {
      s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { // letting the run method throw the error would lose information about args that caused error!
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = {
    forAll(g(_))(f)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop = props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = LinearCongruentialGenerator(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests!")
      case Proved =>
        println(s"+ OK, proved property!")
    }
  }

  private[this] def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(r => Some(g.sample.run(r)))
  }

}
