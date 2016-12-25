package language.preprocessing

import environment.Context
import exceptions.{EscapedSymbolNotFoundException, UnclosedQuotationException}
import io._
import io.streams.{InputStream, ListBufferStream, ResettableStream, StringInputStream}
import language.Alphabet

import scala.collection.mutable.ListBuffer

/**
  * Stateless static preprocessor class, responsible
  * for expanding variables in a given inputString with
  * a given Context
  *
  * Produces ResettableStream of Chars, in which all variables
  * are expanded.
  *
  */
object Preprocessor {
  def process(inputString: String, ctx: Context): ResettableStream[Char] = {
    process(new StringInputStream(inputString), ctx)
  }

  private def process(input: ResettableStream[Char], ctx: Context): ListBufferStream[Char] = {
    val output = new ListBufferStream[Char]()

    while (input.head.isDefined) {
      input.head match {
        case strongQuote if Alphabet.isStrongQuote(strongQuote) =>
          val quoteContent: List[Char] = processStrongQuote(input)
          output.writeAll(quoteContent)
        case weakQuote if Alphabet.isWeakQuote(weakQuote) =>
          val quoteContent: List[Char] = processWeakQuote(input, ctx)
          output.writeAll(quoteContent)
        case backslash if Alphabet.isBackslash(backslash) =>
          val escapedChar = processEscapedChar(input)
          output.writeAll(escapedChar)
        case dollar if Alphabet.isDollar(dollar) =>
          val substitution = processVariable(input, ctx)
          output.writeAll(substitution)
        case justSymbol =>
          output.write(input.read().get)
      }
    }

    output
  }

  private def processWeakQuote(input: ResettableStream[Char], ctx: Context): List[Char] = {
    val result = new ListBuffer[Char]()
    val openingQuote = input.read().get
    result.append(openingQuote)

    while (input.head.isDefined) {
      // read until eof or closing mark,
      // processing escaped chars and substitutions
      input.head match {
        case backslash if Alphabet.isBackslash(backslash) =>
          result.appendAll(processEscapedChar(input))
        case dollar if Alphabet.isDollar(dollar) =>
          result.appendAll(processVariable(input, ctx))
        case quotemark if Alphabet.isWeakQuote(quotemark) =>
          result.append(input.read().get)
          return result.toList
        case justSymbol =>
          result.append(input.read().get)
      }
    }

    // We can get here only if we read opening quotemark, but haven't
    // found closing quotemark until the end of the input.
    throw UnclosedQuotationException()
  }

  private def processEscapedChar(input: InputStream[Char]): List[Char] = {
    val result = ListBuffer[Char]()

    val backslash: Char = input.read().get
    val escapedChar: Option[Char] = input.read()

    if (escapedChar.isEmpty) {
      throw EscapedSymbolNotFoundException()
    }

    result.append(backslash, escapedChar.get)

    result.toList
  }

  private def processStrongQuote(input: ResettableStream[Char]): List[Char] = {
    val result = ListBuffer[Char]()

    val openingQuote: Char = input.read().get
    result.append(openingQuote)

    while (input.head.isDefined) {
      input.head match {
        case backslash if Alphabet.isBackslash(backslash) =>
          result.appendAll(processEscapedChar(input))
        case quotemark if Alphabet.isStrongQuote(quotemark) =>
          result.append(input.read().get)
          return result.toList
        case justSymbol =>
          result.append(input.read().get)
      }
    }

    // We can get here only if we have read opening quotemark, but haven't
    // found closing quotemark until the end of the input.
    throw UnclosedQuotationException()
  }

  private def processVariable(input: ResettableStream[Char], ctx: Context): String = {
    val dollarMark = input.read()
    var varNameChars: List[Char] = Nil

    if (Alphabet.isOpenCurlyBracket(input.head)) {
      // if curly bracket is found, then variable name is everything till the
      // closing bracket
      val openingQuote = input.read()
      varNameChars = input.consumeUntil(Alphabet.isCloseCurlyBracket)
      val closingQuote = input.read()
    } else {
      // else variable name is everything till the whitespace or another dollar or quotation
      varNameChars = input.consumeUntil(maybeChar =>
        Alphabet.isWhitespace(maybeChar) || Alphabet.isDollar(maybeChar) || Alphabet.isQuote(maybeChar)
      )
    }

    val varName: String = varNameChars.mkString

    ctx.getOrElse(varName, "")
  }
}
