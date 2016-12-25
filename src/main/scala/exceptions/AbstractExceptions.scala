package exceptions

/**
  * Represents generic error occurred during interpretation.
  *
  * See `InterpretationExceptions` for more detailed instances
  * of this exception
  */
class InterpretationException(
    message: String = null,
    cause: Throwable = null)
  extends Exception(message, cause)

/**
  * Represents generic error occurred during lexing/parsing stage.
  *
  * See `SyntaxExceptions` for more detailed instances of this exception
  */
class SyntaxError(
    message: String = null,
    cause: Throwable = null)
  extends Exception(message, cause)
