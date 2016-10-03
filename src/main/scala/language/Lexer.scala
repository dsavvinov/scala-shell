package language

import io.{InputStream, ListBufferStream, OutputStream}

import scala.collection.mutable

object Lexer {
  def consume(input: InputStream[Char]): InputStream[Token] = {
    val outputStream = new ListBufferStream[Token]()
    var token: Option[Token] = nextToken(input)
    while (token.isDefined) {
      outputStream.write(token.get)
      token = nextToken(input)
    }
    outputStream
  }

  def nextToken(input: InputStream[Char]): Option[Token] = {
    consumeWhitespaces(input)
    input.mark()
    // Try read some generic sequence of chars
    var token = consumeControlSequence(input)
    if (token.isDefined) {
      input.unmark()
      return token
    }

    // undo unsuccessful read and try read control sequence
    input.reset()
    input.mark()
    token = consumeSymbols(input)
    if (token.isEmpty) {
      input.reset()
      return None
    }
    input.unmark()
    token
  }

  private def consumeWhitespaces(input: InputStream[Char]): Unit = {
    consumeUntil(input, {x => !(Alphabet isWhitespace x)})
  }

  private def consumeSymbols(input: InputStream[Char]): Option[Token] = {
    val word = consumeUntil(input, Alphabet.isWhitespace)

    if (word.isEmpty) {
      return None
    }
    Some(Token(word, Word))
  }

  private def consumeUntil(
                            input: InputStream[Char],
                            isStopSymbol: Option[Char] => Boolean
                          ): String = {
    val sb: StringBuilder = mutable.StringBuilder.newBuilder
    while (!isStopSymbol(input.head) && !input.isAtEnd()) {
      sb append input.read().get
    }
    sb.toString()
  }

  private def consumeControlSequence(input: InputStream[Char]): Option[Token] = {
    /** Control sequence is any reserved by language sequence that affects parsing:
      * - Dash (-, necessary for arguments parsing)
      * - String Literal (some text inside quotes)
      * - Dollar sign ($, used in weak quotes for variables substitution)
      * - Backslash sign (\, used for escaping)
      */
    import input.{read, head}

    if (Alphabet isQuote head) {
      // Read opening quotemark and assign type of quote
      val openQuotemark = read()
      val isStrong = Alphabet isStrongQuote openQuotemark

      // Build predicate that will read until closing quote mark
      val stopPredicate: (Option[Char] => Boolean) =
        if (isStrong) Alphabet.isStrongQuote else Alphabet.isWeakQuote

      val stringLiteral = consumeUntil(input, stopPredicate)
      val closeQuotemark = read()

      // Build content of Literal
      val sb: StringBuilder = mutable.StringBuilder.newBuilder
      sb append openQuotemark.get
      sb append stringLiteral
      sb append closeQuotemark.get

      val tokenType = if (isStrong) StrongQuotation else WeakQuotation
      return Some(Token(sb.toString, tokenType))
    }

    if (Alphabet isPipe head) {
      val pipeSymbol = read()
      return Some(Token(pipeSymbol.get.toString, PipeSeparator))
    }

    if (Alphabet isDash head) {
      val dash = read()
      val word = input.consumeUntil(Alphabet.isWhitespace)
      val result: StringBuilder = mutable.StringBuilder.newBuilder
      result append dash.get
      word.addString(result)
      return Some(Token(result.toString(), CommandOptionToken))
    }

    if (Alphabet isBackslash head) {
      val backslash = read()
      return Some(Token(backslash.get.toString, Backslash))
    }

    None
  }
}
