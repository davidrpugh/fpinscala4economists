package monoids

import adts.Tree


trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}


object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = List.empty[A]
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  // choice of implementation as op must be associative but not necessarily commutative!
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]): Option[A] = o1.orElse(o2)  // o2.orElse(o1)
    def zero: Option[A] = None
  }

  // choice of implementation here since op does not need to be commutative!
  def endoMonoid[A]: Monoid[(A) => A] = new Monoid[(A) => A] {
    def op(f1: (A) => A, f2: (A) => A): (A) => A = f1.compose(f2)  // f1.andThen(f2)
    def zero: (A) => A = identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = {
    as.foldLeft(m.zero)(m.op)
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: (A) => B): B = {
    concatenate(as.map(f), m)
  }

  def foldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: (A) => B): B = {
    if (as.length == 0) {
      m.zero
    } else if (as.length == 1) {
      f(as(0))
    } else {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMap(left, m)(f), foldMap(right, m)(f))
    }
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def isOrdered(as: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1.min(x2), y1.max(y2), p && q && y1 <= y2))
          case (s, None) => s
          case (None, s) => s
        }
      }
      def zero: Option[(Int, Int, Boolean)] = None
    }
    foldMap(as, m)(a => Some((a, a, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(leftStub: String, words: Int, rightStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        val splitWord = r1 + l2
        val w = w1 + w2 + (if (splitWord.isEmpty) 0 else 1)
        Part(l1, w, r2)
    }
    def zero: WC = Stub("")
  }

  def wordCount(input: String): Int = {
    def charToWC(c: Char): WC = {
      if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    }
    foldMap(input.toIndexedSeq, wcMonoid)(charToWC) match {
      case Stub(s) => s.length min 1
      case Part(l, w, r) => (l.length min 1) + w + (r.length min 1)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
      foldMap(as)(f.curried)(endoMonoid)(z)
    }
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
      foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid))(z)
    }
    def foldMap[A, B](as: F[A])(f: (A) => B)(m: Monoid[B]): B = {
      foldRight(as)(m.zero)((a, b) => m.op(f(a), b))
    }
    def concatenate[A](as: F[A])(m: Monoid[A]): A = {
      foldLeft(as)(m.zero)(m.op)
    }
    def toList[A](as: F[A]): List[A] = {
      foldLeft(as)(List.empty[A])((l, a) => a :: l)
    }

  }

  val foldableList: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
      as.foldRight(z)(f)
    }
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
      as.foldLeft(z)(f)
    }
  }

  val foldableOption: Foldable[Option] = new Foldable[Option] {}

  val foldableTree: Foldable[Tree] = new Foldable[Tree] {}

  def product[A, B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A, B)] = {
    new Monoid[(A, B)] {
      def op(t1: (A, B), t2: (A, B)): (A, B) = {
        (m1.op(t1._1, t2._1), m2.op(t1._2, t2._2))
      }
      def zero: (A, B) = (m1.zero, m2.zero)
    }
  }

  def merge[K, V](m: Monoid[V]): Monoid[Map[K, V]] = {
    new Monoid[Map[K, V]] {
      def op(m1: Map[K, V], m2: Map[K, V]): Map[K, V] = {
        m2.foldLeft(m1){ case (acc, (k, v)) =>
          acc.updated(k, m.op(acc.getOrElse(k, m.zero), v))
        }
      }
      def zero: Map[K, V] = Map.empty[K, V]
    }
  }

  def functionMonoid[A, B](m: Monoid[B]): Monoid[(A) => B] = {
    new Monoid[(A) => B] {
      def op(f: (A) => B, g: (A) => B): (A) => B = {
        a => m.op(f(a), g(a))
      }
      def zero: (A) => B = {
        a => m.zero
      }
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMap(as, merge[A, Int](intAddition))(a => Map(a -> 1))
  }

}
