package testing


sealed trait Result {
  def isFalsified: Boolean
}


case class Falsified(failure: FailedCase, success: SuccessCount) extends Result {
  def isFalsified: Boolean = true
}

case object Passed extends Result {
  def isFalsified: Boolean = false
}

case object Proved extends Result {
  def isFalsified: Boolean = false
}
