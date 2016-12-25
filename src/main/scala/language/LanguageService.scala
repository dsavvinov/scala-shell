package language

import environment.Context
import interpretation.Interpreter
import language.AST.SyntaxTree
import language.lexing.Lexer
import language.parsing.Parser
import language.preprocessing.Preprocessor

object LanguageService {
  def buildSyntaxTree(input: String, ctx: Context): SyntaxTree = {

    // Preprocess, expanding substitutions
    val preprocessedInput = Preprocessor.process(input, ctx)

    // Tokenize
    val tokenizedInput = Lexer.consume(preprocessedInput)

    // Build syntax tree
    val tree = Parser.parse(tokenizedInput)

    tree
  }

  def runInput(input: String, ctx: Context): Int = {
    val tree = buildSyntaxTree(input, ctx)

    val result = Interpreter.run(tree, ctx)

    result
  }
}
