package cli

import environment.Context
import exceptions._
import interpretation.Interpreter
import language.lexing.Lexer
import language.parsing.Parser
import language.preprocessing.Preprocessor

object CLI {
  var shouldStop = false
  def main(args: Array[String]): Unit = {
    val ctx = new Context()

    while(!shouldStop) {
      try {
        print("> ")

        // Read raw input
        val input = scala.io.StdIn.readLine()

        // Preprocess, expanding substitutions
        val preprocessedInput = Preprocessor.process(input, ctx)

        // Tokenize
        val tokenizedInput = Lexer.consume(preprocessedInput)

        // Build syntax tree
        val tree = Parser.parse(tokenizedInput)

        // Run parsed tree
        val result = Interpreter.run(tree, ctx)
      } catch {

        case e : ExternalCommandErrorException => {
          println(s"Runtime error running external command: ${e.getMessage}")
          println("stderr:")
          println(e.stderr)
        }

        case e : UnclosedQuotationException =>
          println("Syntax error: unclosed quote found")

        case e : EscapedSymbolNotFoundException =>
          println("Syntax error: backslash at the end of input is found")

        case e : AssignmentValueNotFoundException =>
          println("Syntax error, assignment right-hand side isn't valid: " + e.getMessage)

        case e : SyntaxError =>
          println("Syntax error: " + e.getMessage)

        case e : Throwable =>
          println("Unexpected error!")
          e.printStackTrace()
      }

    }
  }
}
