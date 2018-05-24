package parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}


// one possible implementation of Par...
trait Par[+A] extends ((ExecutorService) => Future[A])


object Par {

  /** Converts a function `f` to one that evaluates its result asynchronously. */
  def asyncF[A,B](f: A => B): A => Par[B] = {
    (a) => lazyUnit(f(a))
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    flatMap(cond)(r => if (r) t else f)
  }

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
    flatMap(key)(k => choices(k))
  }

  def choiceN[A](n: Par[Int])(choices: IndexedSeq[Par[A]]): Par[A] = {
    flatMap(n)(idx => choices(idx))
  }

  /** Delay a parallel computation until its actually needed. */
  def delay[A](p: => Par[A]): Par[A] =
    e => p(e)

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean = {
    p1(e) == p2(e)
  }

  /** Helper function used to execute a callback asynchronously. */
  def eval(e: ExecutorService)(r: => Unit): Unit = {
    e.submit(
      new Callable[Unit] {
        def call(): Unit = r
      }
    )
  }

  def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(p)(f))
  }

  /** Marks a computation for concurrent evaluation. The evaluation won’t actually occur until forced by `run`. */
  def fork[A](p: => Par[A]): Par[A] = {
    (e) => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(e)(p(e)(cb))
    }
  }

  def join[A](p: Par[Par[A]]): Par[A] = {
    (e) => new Future[A] {
      def apply(cb: A => Unit): Unit =
        p(e)(p2 => eval(e) { p2(e)(cb) })
    }
  }

  /** Wraps its unevaluated argument in a `Par` and marks it for concurrent evaluation. */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](p: Par[A])(f: (A) => B): Par[B] = {
    map2(p, unit(()))((a, _) => f(a))
  }

  /** Combines the result of two parallel computations using the function `f` */
  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = {
    (e) => new Future[C] {
      private[parallelism] def apply(cb: (C) => Unit) = {
        // mutable vars to store intermediate results
        var ar: Option[A] = None
        var br: Option[B] = None

        /* actor that awaits both results, combines them with f, and passes the result to cb. */
        val combiner = Actor[Either[A, B]](e) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) =>
              eval(e)(cb(f(a, b)))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) =>
              eval(e)(cb(f(a, b)))
          }
        }

        /* Passes the actor as a continuation to both sides. */
        p1(e)(a => combiner ! Left(a))
        p2(e)(b => combiner ! Right(b))
      }
    }
  }

  /** Combines the result of three parallel computations using the function `f` */
  def map3[A, B, C, D](p1: Par[A], p2: Par[B], p3: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(p1, map2(p2, p3)((b, c) => identity((b, c)))){ case (a, (b, c)) => f(a, b, c) }
  }

  /** Combines the result of four parallel computations using the function `f` */
  def map4[A, B, C, D, E](p1: Par[A], p2: Par[B], p3: Par[C], p4: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(p1, map3(p2, p3, p4)((b, c, d) => identity((b, c, d)))){ case (a, (b, c, d)) => f(a, b, c, d) }
  }

  /** Filters elements of a  list in parallel. */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val ps = as.map(asyncF(a => if (f(a)) List(a) else List.empty))
    map(sequence(ps))(_.flatten)
  }

  /** Map a function `f` over a list in parallel.
    *
    * @note We’ve wrapped our implementation in a call to `fork`. With this implementation, `parMap` will return
    *       immediately, even for a huge input list. When we later call `run`, it will `fork` a single
    *       asynchronous computation which itself spawns `N` parallel computations, and then waits for these
    *       computations to finish, collecting their results into a list.
    */
  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = {
    fork(sequence(as.map(asyncF(f))))
  }

  /** Reduces a sequence of integers in parallel using some binary operation.
    *
    * @note Implementation explicitly calls `fork`.
    */
  def parFold[A](numbers: IndexedSeq[A])(z: A)(op: (A, A) => A): Par[A] = {
    if (numbers.size <= 1)
      unit(numbers.headOption.getOrElse(z))
    else {
      val (left, right) = numbers.splitAt(numbers.length / 2)
      map2(fork(parFold(left)(z)(op)), fork(parFold(right)(z)(op)))(op)
    }
  }

  /** Computes the maximum of a sequence of integers in parallel. */
  def parMax(numbers: IndexedSeq[Int])(z: Int): Par[Int] = {
    parFold(numbers)(z)(_ max _)
  }

  /** Computes the sum of a sequence of integers in parallel. */
  def parSum(numbers: IndexedSeq[Int]): Par[Int] = {
    parFold(numbers)(0)(_ + _)
  }

  /** Extracts a value from a `Par` by actually performing the computation.
    *
    * @note implementation blocks until the value `A` is available.
    */
  def run[A](e: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]()  // mutable but threadsafe reference
    val latch = new CountDownLatch(1)
    p(e){ a => ref.set(a); latch.countDown() }
    latch.await()
    ref.get()
  }

  /** Converts a `List[Par[A]]` to a `Par[List[A]]`. */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(List.empty))((h, t) => map2(h, t)(_ :: _))
  }

  /** Parallel sorting of a list of integers. */
  def sortPar(p: Par[List[Int]]): Par[List[Int]] = {
    map(p)(l => l.sorted)
  }

  /** Creates a computation that immediately results in the value `a`. */
  def unit[A](a: A): Par[A] = {
    (e) => new Future[A] {
      private[parallelism] def apply(cb: (A) => Unit) = cb(a)  // how to implement this using SAM syntax?
    }
  }

}
