package state


case class LinearCongruentialGenerator(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = LinearCongruentialGenerator(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

