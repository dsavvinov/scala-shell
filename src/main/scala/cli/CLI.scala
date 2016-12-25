package cli

import environment.Context
import exceptions._
import interpretation.Interpreter
import language.LanguageService
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

        val result = LanguageService.runInput(input, ctx)
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
