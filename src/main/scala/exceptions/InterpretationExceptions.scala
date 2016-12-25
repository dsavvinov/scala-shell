package exceptions


/**
  * Error that occurred during execution of the external command.
  *
  * If execution of that command ended by throwing exception, then
  * `cause` will contain that exception.
  */
case class ExternalCommandErrorException(
    message: String = null,
    cause: Throwable = null,
    stderr: String = null)
  extends InterpretationException(message, cause)
