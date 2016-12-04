package exceptions


case class UnclosedQuotationException(message: String = null, cause: Throwable = null)
  extends SyntaxError(message, cause)

case class EscapedSymbolNotFoundException(message: String = null, cause: Throwable = null)
  extends SyntaxError(message, cause)

case class IncompleteLexingException(message: String = null, cause: Throwable = null)
  extends SyntaxError(message, cause)

case class AssignmentValueNotFoundException(message: String = null, cause: Throwable = null)
  extends SyntaxError(message, cause)