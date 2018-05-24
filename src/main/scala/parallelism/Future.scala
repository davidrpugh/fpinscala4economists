package parallelism


trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }
