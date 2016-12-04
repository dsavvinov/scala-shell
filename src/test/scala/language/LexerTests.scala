package language

import io.StringInputStream
import language.lexing._
import org.scalatest.FlatSpec

class LexerTests extends FlatSpec {
  "Lexer" should "parse single word into single token with same value" in {
    val input = new StringInputStream("hello")
    val tokens = Lexer
      .consume(input)
      .toList
    assertResult(1) { tokens.length }
    assertResult(WordToken) { tokens.head.tokenType }
    assertResult("hello") { tokens.head.value }
  }

  it should "skip trailing whitespaces" in {
    val input = new StringInputStream("   hello   ")
    val tokens = Lexer.consume(input).toList
    assertResult(1) { tokens.length }
    assertResult("hello") { tokens.head.value }
  }

  it should "parse several whitespace-separated words into several tokens" in {
    val input = new StringInputStream("  hello  world  foo   bar ")
    val tokens = Lexer.consume(input).toList
    assertResult(4) { tokens.length }
    assertResult("hello") { tokens.head.value }
    assertResult("world") { tokens(1).value }
    assertResult("foo") { tokens(2).value }
    assertResult("bar") { tokens(3).value }
  }

  it should "parse empty input into empty stream" in {
    val input = new StringInputStream("")
    val tokens = Lexer.consume(input).toList
    assertResult(0) { tokens.length }
  }

  it should "parse whitespaces into empty stream" in {
    val input = new StringInputStream("               ")
    val tokens = Lexer.consume(input).toList
    assertResult(0) { tokens.length }
  }

  it should "unwrap escaped chars" in {
    val input = new StringInputStream("""\$ foo bar \{ \}""")
    val tokens = Lexer.consume(input).toList

    assertResult (5) { tokens.length }
    assertResult (Token("$", WordToken)) { tokens.head }
    assertResult (Token("foo", WordToken)) { tokens(1) }
    assertResult (Token("bar", WordToken)) { tokens(2) }
    assertResult (Token("{", WordToken)) { tokens(3) }
    assertResult (Token("}", WordToken)) { tokens(4) }
  }

  it should "parse quotations into StringLiteral token" in {
    val input = new StringInputStream("""'strong quote' "weak quote"""".stripMargin)

    val tokens = Lexer.consume(input).toList

    assertResult(2) { tokens.length }
    assertResult(Token("strong quote", StringLiteralToken)) { tokens.head }
    assertResult(Token("weak quote", StringLiteralToken)) { tokens(1) }
  }

  it should "unwrap escaped chars in literals" in {
    val input = new StringInputStream(
      """'strong \' quote \' with escapes',""" +
      """"weak \" quote with \" escapes"""".stripMargin)

    val tokens = Lexer.consume(input).toList

    assertResult(3) { tokens.length }
    assertResult(Token("strong \' quote \' with escapes", StringLiteralToken)) { tokens.head }
    assertResult(Token(",", WordToken)) { tokens(1) }
    assertResult(Token("weak \" quote with \" escapes", StringLiteralToken)) { tokens(2) }
  }
}
