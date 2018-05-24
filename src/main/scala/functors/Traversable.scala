package functors

import adts.{Branch, Leaf, Tree}
import monads.Monad


trait Traversable[F[_]] extends Functor[F] {

  // Exercise 12.14
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    traverse[Monad.Identity, A, B](fa)(f)(Monad.identity)
  }

  def sequence[G[_], A](fga: F[G[A]])(implicit ev: Applicative[G]): G[F[A]] = {
    traverse(fga)(identity)
  }

  def traverse[G[_], A, B](fa: F[A])(f: (A) => G[B])(implicit ev: Applicative[G]): G[F[B]]

}


object Traversable {

  val list: Traversable[List] = {
    new Traversable[List] {
      def traverse[G[_], A, B](as: List[A])(f: (A) => G[B])(implicit ev: Applicative[G]): G[List[B]] = {
        as.foldRight(ev.unit(List.empty[B]))((a, gbs) => ev.map2(f(a), gbs)(_ :: _))
      }
    }
  }

  val option: Traversable[Option] = {
    new Traversable[Option] {
      def traverse[G[_], A, B](o: Option[A])(f: (A) => G[B])(implicit ev: Applicative[G]): G[Option[B]] = {
        o match {
          case Some(a) => ev.map(f(a))(b => Some(b))
          case None => ev.unit(None)
        }
      }
    }
  }

  val tree: Traversable[Tree] = {
    new Traversable[Tree] {
      def traverse[G[_], A, B](as: Tree[A])(f: (A) => G[B])(implicit ev: Applicative[G]): G[Tree[B]] = {
        as match {
          case Leaf(a) => ev.map(f(a))(b => Leaf(b))
          case Branch(left, right) => ev.map2(traverse(left)(f), traverse(right)(f))((l, r) => Branch(l, r))
        }
      }
    }

  }


}
