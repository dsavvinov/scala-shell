package language

import io.StringInputStream
import language.lexing.Lexer
import language.AST._
import language.parsing.Parser
import org.scalatest.FlatSpec

class ParserTests extends FlatSpec {
  import language.AST.ASTImplicits._

  "Parser" should "parse single command into command node" in {
    val input = new StringInputStream("cat")
    val tokens = Lexer.consume(input)
    val tree = Parser.parse(tokens)

    val answerTree = new SyntaxTree(
      Program() {
        CommandExpression("cat")
      }
    )

    assert(answerTree == tree)
  }

  it should "parse single command with some arguments into command node with proper childs" in {
    val input = new StringInputStream("""echo -f -e "file1.txt" file2!""")
    val tokens = Lexer.consume(input)
    val tree = Parser.parse(tokens)

    val answerTree = new SyntaxTree(
      Program() {
        CommandExpression("echo") {
            Word("-f")         \
            Word("-e")         \
            Word("file1.txt")  \
            Word("file2!")
        }
      }
    )
    assert(answerTree == tree)
  }

  it should "parse trivial pipe into pipe-node" in {
    val input = new StringInputStream("""cat "file1 file2" | wc""")
    val tokens = Lexer.consume(input)
    val tree = Parser.parse(tokens)

    val answerTree = new SyntaxTree(
      Program() {
        PipeExpression() {
          CommandExpression("cat") {
            Word("file1 file2")
          } \
          CommandExpression("wc")
        }
      }
    )

    assert(answerTree == tree)
  }

  it should "parse chained pipes into node with a lot of childs" in {
    val input = new StringInputStream("""cat file1 file2 | wc -l | wc '-d' -f""")
    val tokens = Lexer.consume(input)
    val tree = Parser.parse(tokens)

    val answerTree = new SyntaxTree(
      Program() {
        PipeExpression() {
          CommandExpression("cat") {
            Word("file1")   \
            Word("file2")
          } \
          CommandExpression("wc") {
            Word("-l")
          } \
          CommandExpression("wc") {
            Word("-d") \
            Word("-f")
          }
        }
      }
    )
    assert (answerTree == tree)
  }
}
