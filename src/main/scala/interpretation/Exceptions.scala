package interpretation

case class InvalidArgumentTypeException(
    message: String = null,
    cause: Throwable = null
  )
  extends Exception(message, cause) {
}