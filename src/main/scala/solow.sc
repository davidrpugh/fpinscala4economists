/** Equation of motion for capital (per unit effective labor). */
def solowModel(f: Double => Double)(delta: Double, g: Double, n: Double, s: Double)(k: Double): Double = {
  s * f(k) - (delta + n + g) * k
}

/** Cobb-Douglas production. */
def cobbDouglas(alpha: Double)(k: Double): Double = {
  math.pow(k, alpha)
}

solowModel(k => math.pow(k, 0.33))(0.05, 0.05, 0.05, 0.15)(1.0)


