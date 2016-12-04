package language.lexing

/**
  * Sealed trait of possible tokens.
  */
sealed trait TokenType

case object StringLiteralToken  extends TokenType
case object PipeCharToken       extends TokenType
case object AssignmentCharToken extends TokenType
case object EscapedSymbolToken  extends TokenType
case object WordToken           extends TokenType
case object WhitespaceToken     extends TokenType

case class Token(value: String, tokenType: TokenType) { }
