package exceptions


case class ExternalCommandErrorException(
    message: String = null,
    cause: Throwable = null,
    stderr: String = null)
  extends InterpretationException(message, cause)
