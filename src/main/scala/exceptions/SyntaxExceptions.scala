package exceptions


/**
  * Exception is thrown when unclosed quotation is found in input string
  */
case class UnclosedQuotationException(message: String = null, cause: Throwable = null)
  extends SyntaxError(message, cause)

/**
  * Exception is thrown when backslash is found, but then EOF follows.
  */
case class EscapedSymbolNotFoundException(message: String = null, cause: Throwable = null)
  extends SyntaxError(message, cause)

/**
  * Exception is thrown when some non-empty suffix of input can not be lexed in any correct way.
  */
case class IncompleteLexingException(message: String = null, cause: Throwable = null)
  extends SyntaxError(message, cause)

/**
  * Exception is thrown when assignment char is found (`=`), but then EOF follows.
  */
case class AssignmentValueNotFoundException(message: String = null, cause: Throwable = null)
  extends SyntaxError(message, cause)