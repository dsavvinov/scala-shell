package language.lexing

import exceptions.{EscapedSymbolNotFoundException, IncompleteLexingException, UnclosedQuotationException}
import io.{InputStream, ListBufferStream, ResettableStream}
import language._

/**
  * Lexer class. Consumes ResettableStream of Chars and tokenizes it,
  * returning ResettableStream of tokens.
  * Each token is a subtype of Token sealed trait.
  *
  */
object Lexer {
  def consume(input: ResettableStream[Char]): ResettableStream[Token] = {
    val outputStream = new ListBufferStream[Token]()
    var token: Option[Token] = nextToken(input)

    while (token.isDefined) {
      outputStream.write(token.get)
      token = nextToken(input)
    }

    /* Note that we don't check there if the end of the input was really reached,
     because every input except for ones with unmatched quotes is tokenizable */
    outputStream
  }

  /**
    * Try to get next token.
    *
    * Returns None if next token can't be parsed.
    */
  private def nextToken(input: ResettableStream[Char]): Option[Token] = {
    while (input.head.isDefined) {
      input.head match {

        case assignmentChar if Alphabet.isAssignment(assignmentChar) =>
          return Some(Token(input.read().get.toString, AssignmentCharToken))

        case pipeChar if Alphabet.isPipe(pipeChar) =>
          return Some(Token(input.read().get.toString, PipeCharToken))

        case quotemark if Alphabet.isQuote(quotemark) =>
          return consumeStringLiteral(input)

        case whitespace if Alphabet.isWhitespace(whitespace) =>
          // Currently, whitespaces are ignored during lexing and are invisible to parser
          input.read()

        case genericSymbol =>
          return consumeWord(input)
      }
    }
    None
  }

  private def consumeEscapedSymbol(input: ResettableStream[Char]): Option[Token] = {
    val backslash = input.read()

    if (input.head.isEmpty) {
      // Error: backslash at the end of the input
      throw EscapedSymbolNotFoundException()
    }

    val escapedSymbol: Char = input.read().get
    Some(Token(escapedSymbol.toString, EscapedSymbolToken))
  }

  private def consumeStringLiteral(input: ResettableStream[Char]): Option[Token] = {
    input.mark()

    val openingQuote = input.read()
    val quoteContent = StringBuilder.newBuilder

    // Consume string literal content
    while(input.head.isDefined) {
      input.head match {
        case backslash if Alphabet.isBackslash(backslash) =>
          val escapedSymbol: Option[Token] = consumeEscapedSymbol(input)
          quoteContent append escapedSymbol.get.value

        case quote if Alphabet.isQuote(quote) =>
          val closingQuote = input.read()
          input.unmark()
          return Some(Token(quoteContent.mkString, StringLiteralToken))

        case symbol =>
          quoteContent append input.read().get
      }
    }

    // We can get here only if the end of input is reached and closing quote wasn't found
    throw UnclosedQuotationException()
  }

  private def consumeWord(input: ResettableStream[Char]): Option[Token] = {
    val word = StringBuilder.newBuilder

    while (input.head.isDefined) {
      input.head match {
        case backslash if Alphabet.isBackslash(backslash) =>
          val escapedChar: Option[Token] = consumeEscapedSymbol(input)
          word append escapedChar.get.value
        case stopSymbol if
            Alphabet.isPipe(stopSymbol) ||
            Alphabet.isQuote(stopSymbol) ||
            Alphabet.isAssignment(stopSymbol) ||
            Alphabet.isWhitespace(stopSymbol) =>
          return Some(Token(word.mkString, WordToken))
        case genericSymbol =>
          word append input.read().get
      }
    }

    Some(Token(word.mkString, WordToken))
  }
}
