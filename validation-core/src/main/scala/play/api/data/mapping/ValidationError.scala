package jto.validation

/**
 * A validation error.
 *
 * @param message the error message
 * @param args the error message arguments
 */
case class ValidatedError(messages: Seq[String], args: Any*) {
  lazy val message = messages.last
}

object ValidatedError {
  def apply(message: String, args: Any*) = new ValidatedError(Seq(message), args: _*)
}
