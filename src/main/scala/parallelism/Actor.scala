package parallelism

import java.util.concurrent.{Callable, ExecutorService}
import java.util.concurrent.atomic.AtomicReference


final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw _) {
  self =>

  /** Pass the message `a` to the mailbox of this actor. */
  def apply(a: A): Unit = {
    this ! a
  }

  /** Alias for `apply`. */
  def !(a: A): Unit = {
    val node = new Node(a)
    head.getAndSet(node).lazySet(node)
    trySchedule()
  }

  /* private implementation details. */
  private val tail = new AtomicReference(new Node())
  private val head = new AtomicReference(tail.get)
  private val suspended = new AtomicReference(1)

  private def trySchedule(): Unit = {
    if (suspended.compareAndSet(1, 0)) {
      schedule()
    }
  }

  private def schedule(): Unit = {
    strategy(act())
  }

  private def act(): Unit = {
    val t = tail.get()
    val n = batchHandle(t, 1024)
    if ( n ne t) {
      n.a = null.asInstanceOf[A]
      tail.lazySet(n)
      schedule()
    } else {
      suspended.set(1)
      if (n.get() ne null) trySchedule()
    }
  }

  @annotation.tailrec
  private def batchHandle(node: Node, i: Int): Node = {
    val current = node.get()
    if (current ne null) {
      try {
        handler(current.a)
      } catch {
        case ex: Throwable => onError(ex)
      }
      if (i > 0) batchHandle(node, i - 1) else current
    } else {
      node
    }
  }

  private class Node(var a: A = null.asInstanceOf[A]) extends AtomicReference[Node]

}


object Actor {

  /** Create an `Actor` backed by the given `ExecutorService`. */
  def apply[A](es: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw _): Actor[A] =
    new Actor(Strategy.fromExecutorService(es))(handler, onError)

}


/**
  * Provides a function for evaluating expressions, possibly asynchronously.
  * The `apply` function should typically begin evaluating its argument
  * immediately. The returned thunk can be used to block until the resulting `A`
  * is available.
  */
trait Strategy {
  def apply[A](a: => A): () => A
}


object Strategy {

  /**
    * We can create a `Strategy` from any `ExecutorService`. It's a little more
    * convenient than submitting `Callable` objects directly.
    */
  def fromExecutorService(e: ExecutorService): Strategy = new Strategy {
    def apply[A](a: => A): () => A = {
      val f = e.submit { new Callable[A] { def call(): A = a} }
      () => f.get
    }
  }

  /**
    * A `Strategy` which begins executing its argument immediately in the calling thread.
    */
  def sequential: Strategy = new Strategy {
    def apply[A](a: => A): () => A = {
      val r = a
      () => r
    }
  }
}
