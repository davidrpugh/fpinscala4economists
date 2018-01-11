package state


trait RNG {

  def nextInt: (Int, RNG)

}


object RNG {

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (n, rng2) = rng.nextInt
    (n % 2 == 0, rng2)
  }

  /** Exercise 6.1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /** Exercise 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    val(i, r) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /** Exercise 6.3 */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  /** Exercise 6.4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @annotation.tailrec
    def loop(remaining: Int, prng: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (remaining == 0) {
        (xs, prng)
      } else {
        val (x, prng2) = prng.nextInt
        loop(remaining - 1, prng2, x :: xs)
      }
    }
    loop(count, rng, List())

  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => val (a, rng2) = s(rng); (f(a), rng2)
  }

  /** Exercise 6.5 */
  def double2: Rand[Double] = {
    map(nonNegativeInt)(int => int / (Int.MaxValue.toDouble + 1))
  }

  /** Exercise 6.6 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => val (a, rng2) = ra(rng); val (b, rng3) = rb(rng2); (f(a, b), rng3)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(int => int - int % 2 )
  }

  /** Exercise 6.7 */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((ra, rb) => map2(ra, rb)((a, as) => a :: as))
  }

}