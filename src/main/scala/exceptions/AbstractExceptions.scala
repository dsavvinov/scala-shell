package exceptions
class InterpretationException(
    message: String = null,
    cause: Throwable = null)
  extends Exception(message, cause)

class SyntaxError(
    message: String = null,
    cause: Throwable = null)
  extends Exception(message, cause)
