import state.{LinearCongruentialGenerator, RNG, Rand}

def randomPair(rng: RNG): ((Int,Int), RNG) = {
  val (i1,rng2) = rng.nextInt
  val (i2,rng3) = rng2.nextInt
  ((i1,i2), rng3)
}

val rng = LinearCongruentialGenerator(42)
randomPair(rng)


val (v1, rng2) = RNG.nonNegativeInt(rng)
val (v2, _) = RNG.nonNegativeInt(rng2)
(v1, v2)
RNG.double3(rng)

val int: Rand[Int] = (rng) => rng.nextInt



def _ints(count: Int): Rand[List[Int]] = RNG.sequence(List.fill(count)(int))
_ints(5)(rng2)


RNG.nonNegativeEven(rng2)

RNG.sequence(List.fill(5)(int))

RNG.double2(rng)