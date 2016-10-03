package language

import io.InputStream
import language.AST._

// TODO: Parse arguments properly (now arguments should be quoted)

object ParseResult {
  def Nothing[T] = new ParseResult[T](None)
  def Just[T](value: T) = new ParseResult[T](Some(value))
}

class ParseResult[+T] (val result: Option[T]) {
  def get = result.get
  def isDefined = result.isDefined
  def isEmpty = result.isEmpty

  def this(value: T) = {
    this(Some(value))
  }
}

object Parser {
  import ParseResult._

  def parse(input: InputStream[Token]): SyntaxTree = {
    /** AST root is node of type Program, and it's childrens are Expressions */
    val root = new Program
    while (input.head.isDefined) {
      root append expression(input).get
    }
    new SyntaxTree(root)
  }

  def expression(input: InputStream[Token]): ParseResult[Expression] = {
    /** Each expression is a:
      * - pipe
      * - single command
      * Note that order matters: because E1 | E2 could be erroneously parsed first as
      * Command(E1), but then parsing will fail, so case with pipe should go first
      */

    // Pipe-case
    val pipeExpr: ParseResult[PipeExpression] = pipe(input)
    if (pipeExpr.isDefined) {
      return Just(pipeExpr.get)
    }

    // Cmd-case
    val maybeCmd: ParseResult[CommandExpression] = command(input)
    if (maybeCmd.isDefined) {
      return Just(maybeCmd.get)
    }

    Nothing
  }

  def command(input: InputStream[Token]): ParseResult[CommandExpression] = {
    /** Each command is a:
      * - command name (single word from pre-defined list of possible commands)
      * - command arguments
      */
    val commandName: Option[String] = input.read().map { _.value }
    if (!(Alphabet isCommand commandName)) {
      input.unread()
      return Nothing
    }

    val curNode = CommandExpression(commandName.get)
    var arg: ParseResult[Argument] = argument(input)
    while (arg.isDefined) {
      curNode append arg.get
      arg = argument(input)
    }

    Just(curNode)
  }

  def pipe(input: InputStream[Token]): ParseResult[PipeExpression] = {
    /** Each pipe is a:
      * - command, followed by
      * - pipe symbol, followed by
      * - another pipe OR single last command
      * Note: order matters, "another pipe"-case should go
      *   before "last command"-case
      */
    input.mark()
    val curNode = PipeExpression()

    // Try parse command
    val commandExpr: ParseResult[CommandExpression] = command(input)
    if (commandExpr.isEmpty) {
      input.reset()
      return Nothing
    }

    // Then try parse token
    val pipeToken: Option[Token] = input.read()
    if (!(pipeToken exists { _.tokenType == PipeSeparator})) {
      input.reset()
      return Nothing
    }

    // Then, first try to parse another chained pipe
    val pipeTailExpr: ParseResult[PipeExpression] = pipe(input)
    if (pipeTailExpr.isDefined) {
      // If succeeded, append it as second argument to pipe
      curNode append commandExpr.get
      curNode append pipeTailExpr.get

      // Remove pushed mark from stream
      input.unmark()
      return Just(curNode)
    }

    // If chained pipe wasn't parsed successfully, try to find the last command in the pipe
    val lastCommand: ParseResult[CommandExpression] = command(input)
    if (lastCommand.isEmpty) {
      input.reset()
      return Nothing
    }

    curNode append commandExpr.get
    curNode append lastCommand.get
    input.unmark()
    Just(curNode)
  }

  def argument(input: InputStream[Token]): ParseResult[Argument] = {
    /** Argument is a:
      * - Strong quotation (in single quote marks)
      * - Weak quotation (in double quote marks)
      * - Option (starts with a dash)
      * */

    input.mark()

    // Try parse as a strong quotation
    val strongQuote = strongQuotation(input)
    if (strongQuote.isDefined) {
      input.unmark()
      return strongQuote
    }

    // Try parse as a weak quotation
    val weakQuote = weakQuotation(input)
    if (weakQuote.isDefined) {
      input.unmark()
      return weakQuote
    }

    // Finally, try parse as an option for a command
    val option = commandOption(input)
    if (option.isDefined) {
      input.unmark()
      return option
    }

    // Nothing is parsed, reset the stream
    input.reset()
    Nothing
  }

  def strongQuotation(input: InputStream[Token]): ParseResult[StrongQuotationNode] = {
    if (!(input.head exists { _.tokenType == StrongQuotation})) {
      return Nothing
    }
    val quotationToken = input.read()
    Just(StrongQuotationNode(quotationToken.get.value))
  }

  def weakQuotation(input: InputStream[Token]): ParseResult[WeakQuotationNode] = {
    if (!(input.head exists { _.tokenType == WeakQuotation})) {
      return Nothing
    }
    val quotationToken = input.read()
    // Strip quotes
    val quotedString = quotationToken.get.value
    val quoteContent = quotedString.drop(1).dropRight(1)
    Just(WeakQuotationNode(quoteContent))
  }

  def commandOption(input: InputStream[Token]): ParseResult[CommandOption] = {
    if (!(input.head exists { _.tokenType == CommandOptionToken})) {
      return Nothing
    }
    val commandOption = input.read()
    Just(CommandOption(commandOption.get.value))
  }
}
