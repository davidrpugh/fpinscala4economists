sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /** Exercise 3.25 */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /** Exercise 3.26 */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  /** Exercise 3.27 */
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left) max depth(right)
  }

  /** Exercise 3.28 */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /** Exercise 3.29 */
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left), fold(right))
  }

  def size2[A](tree: Tree[A]): Int =  {
    fold(tree)(a => 1)((b1, b2) => 1 + b1 + b2)
  }

  def maximum2(tree: Tree[Int]): Int = {
    fold(tree)(a => a)((b1, b2) => b1 max b2)
  }

  def depth2[A](tree: Tree[A]): Int = {
    fold(tree)(a => 0)((b1, b2) => 1 + b1 + b2)
  }

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2))
  }

}