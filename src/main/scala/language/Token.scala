package language

/**
  * Created by dsavv on 12.09.2016.
  */
sealed trait TokenType

case object Word            extends TokenType
case object StrongQuotation extends TokenType
case object WeakQuotation   extends TokenType
case object PipeSeparator   extends TokenType
case object CommandOptionToken            extends TokenType
case object Dollar          extends TokenType
case object Backslash       extends TokenType

case class Token(value: String, tokenType: TokenType) { }
