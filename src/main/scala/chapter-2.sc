/** Exercise 2.1
  *
  * Computes the a Fibonacci number.
  * 
  * @param n the Fibonacci number to compute.
  * @return a Fibonacci number.
  */
def fibonacci(n: Int): Int = {

  @annotation.tailrec
  def go(x: Int, w: Int, y: Int): Int = {
    if (x == 0) w else if (x == 1) y else go(x - 1, y, w + y)
  }

  go(n, 0, 1)
}

val expected = Vector(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
val actual = for (n <- 1 to 10) yield fibonacci(n)
assert(expected == actual)


/** Exercise 2.2
  *
  * Checks whether an array is sorted according to a given comparison function.
  *
  * @param as the array to check.
  * @param ordered a comparison function.
  * @note the comparison function should return true if its first argument is "less than" its second.
  * @return true if the array is sorted, false otherwise.
  */
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

  @annotation.tailrec
  def loop(i: Int): Boolean = {
    if (i >= as.length - 1) true else if (!ordered(as(i), as(i + 1))) false else loop(i + 1)
  }

  loop(0)
}

val sorted = Array(-1, 0, 2, 3, 4)
val unsorted = Array("foo", "baz", "bar")
assert(isSorted(sorted, (x: Int, y: Int) => x < y))
assert(!isSorted(unsorted, (x: String, y: String) => x < y))


/** Exercise 2.3
  *
  * Converts a function f of two arguments into a function of one argument that partially applies f.
  *
  * @param f a function to curry.
  * @tparam A type of first input to the function f (and the desired input type of the returned function).
  * @tparam B type of the second input to the function f.
  * @tparam C return type of the function f.
  * @return a function mapping instances of type A to a function that maps instances of type B to instances of type C.
  */
def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  a => b => f(a, b)
}


/** Exercise 2.4
  *
  * Reverses the transformation of curry.
  *
  * @param f a function to un-curry.
  * @tparam A type of input to the function f (and the type of first input to the returned function).
  * @tparam B type of input to the function returned by f (and the type of the second input to the returned function).
  * @tparam C return type of the function f (and the type of the output of the returned function).
  * @return a function mapping instances of type A and type B to instances of type C.

  * @note Since => associates to the right, A => (B => C) can be written as A => B => C.
  */
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}


/** Exercise 2.5
  *
  * A higher-order function that composes to functions f and g.
  *
  * @param f a function mapping instance of type B to instances of type C.
  * @param g a function mapping instances of type A to instances of type B.
  * @tparam A the input type of the function g.
  * @tparam B the input type of the function f.
  * @tparam C the return type of the function f (and the output type of the returned function)
  * @return a function mapping instances of type A to instances of type C.
  */
def compose[A,B,C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}