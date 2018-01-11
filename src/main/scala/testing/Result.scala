package testing


sealed trait Result {
  def isFalsified: Boolean
}


case object Passed extends Result {
  def isFalsified: Boolean = false
}


case class Falsified(failure: FailedCase, success: SuccessCount) extends Result {
  def isFalsified: Boolean = true
}
