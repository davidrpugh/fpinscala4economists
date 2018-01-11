import laziness.Stream


/** Exercise 5.10 */
def fibonacci: Stream[Int] = {

  def go(current: Int, next: Int): Stream[Int] = {
    Stream.cons(current, go(next, current + next))
  }
  go(0, 1)

}

/** Exercise 5.12 */
def fibonacci2: Stream[Int] = {
  Stream.unfold((0, 1)){ case (m, n) => Some(m, (n, m + n)) }
}

def logisticMap(r: Double)(t0: Int, x0: Double): Stream[Double] = {
  Stream.unfold((t0, x0)){ case (t, x) => Some(r * x * (1 - x), (t + 1, r * x * (1 - x))) }
}


val s = Stream(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
s.drop(3).toList
s.take(3).toList
s.take2(3).toList
s.zipWith(s)(_ + _).toList

// example of an infinite stream
val ones: Stream[Int] = Stream.constant(1)
ones.take(5).toList
ones.map(_ + 1).exists(_ % 2 == 0)
ones.forAll(_ != 1)

Stream.from(42).take(5).toList

fibonacci.take(10).toList
fibonacci2.take(10).toList

Stream(1,2,3) startsWith Stream(1,2)
Stream(1,2,3).tails.toList.map(_.toList)

logisticMap(1.5)(0, 0.5).take(100).toList


