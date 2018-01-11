package object state {

  type Rand[+A] = (RNG) => (A, RNG)

}
