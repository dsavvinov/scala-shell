package language

/**
  * Created by dsavv on 13.09.2016.
  */
object Alphabet {
  val whitespaceChars = List(' ')
  val commandNames: List[String] = List("cat", "echo", "wc", "pwd", "exit")
  val pipeChar = '|'
  val strongQuoteChar = '\''
  val weakQuoteChar = '\"'
  val dashChar = '-'
  val dollarSign = '$'
  val backslashSign = '\\'
  val openingCurlyBracket = '{'
  val closingCurlyBracket = '}'

  def isWhitespace(char: Option[Char]): Boolean = {
    char exists { whitespaceChars contains _ }
  }

  def isSymbol(char: Option[Char]): Boolean = !isWhitespace(char)

  def isPipe(char: Option[Char]): Boolean = {
    char contains pipeChar
  }

  def isCommand(name: Option[String]): Boolean = {
    name exists { commandNames contains _ }
  }

  def isDash(char: Option[Char]): Boolean = {
    char contains dashChar
  }

  def isDollar(char: Option[Char]): Boolean = {
    char contains dollarSign
  }

  def isBackslash(char: Option[Char]): Boolean = {
    char contains backslashSign
  }

  def isQuote(char: Option[Char]): Boolean = {
    isStrongQuote(char) || isWeakQuote(char)
  }

  def isStrongQuote(char: Option[Char]): Boolean = {
    char contains strongQuoteChar
  }

  def isWeakQuote(char: Option[Char]): Boolean = {
    char contains weakQuoteChar
  }

  def recognizeAsStrongQuote(text: Option[String]): Boolean = {
    text exists { it => (it startsWith "\'") && (it endsWith "\'") }
  }

  def recognizeAsWeakQuote(text: Option[String]): Boolean = {
    text exists { it => (it startsWith "\"") && (it endsWith "\"") }
  }

  def recognizeAsOption(text: Option[String]): Boolean = {
    text exists { _ startsWith "-" }
  }

  def isOpenCurlyBracket(char: Option[Char]): Boolean = {
    char contains openingCurlyBracket
  }

  def isCloseCurlyBracket(char: Option[Char]): Boolean = {
    char contains closingCurlyBracket
  }
}
