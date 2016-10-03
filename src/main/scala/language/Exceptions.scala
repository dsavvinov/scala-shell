package language

case class ParseException(
                          message: String = null,
                          cause: Throwable = null
                         )
  extends Exception(message, cause) {
}