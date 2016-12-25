package language.parsing

import exceptions.{AssignmentValueNotFoundException, SyntaxError}
import io.ResettableStream
import io.streams.ResettableStream
import language.lexing._
import language.AST._

/**
  * Stateless static parser that consumes Resettable Input Stream
  * of tokens and produces SyntaxTree.
  */
object Parser {
  // Allows to drop noisy "ParseResult." in calls of "ParseResult.Just()"
  import ParseResult._

  def parse(input: ResettableStream[Token]): SyntaxTree = {
    /** AST root is node of type Program, and it's childrens are Expressions */
    val root = new Program
    while (input.head.isDefined) {
      val maybeExpression = expression(input)

      /* If parser failed to parse expression and input still
         not at the end, then input has syntax errors
       */
      if (maybeExpression.isEmpty) {
        throw new SyntaxError("Unexpected token " + input.head.toString)
      }

      root append maybeExpression.get
    }
    new SyntaxTree(root)
  }

  private def expression(input: ResettableStream[Token]): ParseResult[Expression] = {
    /** Each expression is a:
      * - assignment
      * - pipe
      * - single command
      * Note that order matters: because E1 | E2 could be erroneously parsed first as
      * Command(E1), but then parsing will fail, so case with pipe should go first
      */

    val assignmentExpr : ParseResult[AssignmentExpression] = assignment(input)
    if (assignmentExpr.isDefined) {
      return Just(assignmentExpr.get)
    }

    val pipeExpr: ParseResult[PipeExpression] = pipe(input)
    if (pipeExpr.isDefined) {
      return Just(pipeExpr.get)
    }

    val maybeCmd: ParseResult[CommandExpression] = command(input)
    if (maybeCmd.isDefined) {
      return Just(maybeCmd.get)
    }

    Nothing
  }

  private def assignment(input: ResettableStream[Token]): ParseResult[AssignmentExpression] = {
    /**
      * Assignment is a:
      * - variable name (single word)
      * - assignment char (AssignmentCharToken)
      * - value (single word or token)
      */

    input.mark()

    val varName: ParseResult[Word] = word(input)
    if (varName.isEmpty) {
      input.reset()
      return Nothing
    }

    if (assignmentSign(input).isEmpty) {
      input.reset()
      return Nothing
    }

    /* At this moment, we have already read variable name and "=",
       so assignment have to be finished. Therefore, if we've failed
       in reading word, we are throwing exception instead of quiet rollback
     */
    val value: ParseResult[Word] = word(input)
    if (value.isEmpty) {
      throw AssignmentValueNotFoundException(s"expected Word-token, found ${input.head.toString}")
    }

    val assignmentNode = AssignmentExpression()
    assignmentNode.append(varName.get)
    assignmentNode.append(value.get)

    input.unmark()
    Just(assignmentNode)
  }


  private def command(input: ResettableStream[Token]): ParseResult[CommandExpression] = {
    /** Each command is a:
      * - command name - just a single word
      * - command arguments - list of word of arbitrary length
      */

    input.mark()

    val commandName = word(input)
    if (commandName.isEmpty) {
      input.reset()
      return Nothing
    }

    val curNode = CommandExpression(commandName.get.value)

    var arg: ParseResult[Node] = word(input)
    while (arg.isDefined) {
      curNode append arg.get
      arg = word(input)
    }

    input.unmark()
    Just(curNode)
  }

  private def pipe(input: ResettableStream[Token]): ParseResult[PipeExpression] = {
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

    // Then try parse pipeSymbol
    if (pipeSymbol(input).isEmpty) {
      input.reset()
      return Nothing
    }

    // Then, first try to parse another chained pipe
    val pipeTailExpr: ParseResult[PipeExpression] = pipe(input)
    if (pipeTailExpr.isDefined) {
      // If succeeded, append its childs to ours
      curNode append commandExpr.get
      curNode appendAll pipeTailExpr.get.commands

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

  def word(input: ResettableStream[Token]): ParseResult[Word] = {
    /**
      * Word (as Node) is just a single WordToken or single StringLiteralToken
      */
    input.mark()

    val wordToken = input.read()
    if (!wordToken.exists { _.tokenType == WordToken } &&
        !wordToken.exists { _.tokenType == StringLiteralToken}) {
      input.reset()
      return Nothing
    }

    val wordNode = Word(wordToken.get.value)
    input.unmark()
    Just(wordNode)
  }

  def assignmentSign(input: ResettableStream[Token]): ParseResult[Word] = {
    input.mark()
    val assignmentToken: Option[Token] = input.read()
    if (! assignmentToken.exists { _.tokenType == AssignmentCharToken }) {
      input.reset()
      return Nothing
    }

    val assignmentNode = Word(assignmentToken.get.value)

    input.unmark()
    Just(assignmentNode)
  }

  def pipeSymbol(input: ResettableStream[Token]): ParseResult[Word] = {
    val pipeCharToken: Option[Token] = input.read()
    input.mark()

    if (!pipeCharToken.exists { _.tokenType == PipeCharToken }) {
      input.reset()
      return Nothing
    }

    input.unmark()
    Just(Word(pipeCharToken.get.value))
  }
}


/**
  * Option-like container that can hold value or
  * indicate absence of value.
  *
  */
class ParseResult[+T] (val result: Option[T]) {
  def get = result.get
  def isDefined = result.isDefined
  def isEmpty = result.isEmpty

  def this(value: T) = {
    this(Some(value))
  }
}

/**
  * Utility methods for less verbose construction of
  * ParseResults (thanks to implicit type inference
  * and scope importing)
  */
object ParseResult {
  def Nothing[T] = new ParseResult[T](None)
  def Just[T](value: T) = new ParseResult[T](Some(value))
}
