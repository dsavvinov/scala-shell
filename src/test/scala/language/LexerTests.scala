package language

import io.StringInputStream
import org.scalatest.FlatSpec

class LexerTests extends FlatSpec {
  "Lexer" should "lex single word into single token with same value" in {
    val input = new StringInputStream("hello")
    val tokens = Lexer
      .consume(input)
      .toList
    assertResult(1) { tokens.length }

  }

  it should "skip trailing whitespaces" in {
    val input = new StringInputStream("   hello   ")
    val tokens = Lexer.consume(input).toList
    assertResult(1) { tokens.length }
    assertResult("hello") { tokens.head.value}
  }

  it should "parse several whitespace-separated words into several tokens" in {
    val input = new StringInputStream("  hello  world  i  love you  ")
    val tokens = Lexer.consume(input).toList
    assertResult(5) { tokens.length }
    assertResult("hello") { tokens.head.value }
    assertResult("world") { tokens(1).value }
    assertResult("i") { tokens(2).value }
    assertResult("love") { tokens(3).value }
    assertResult("you") { tokens(4).value }
  }

  it should "parse empty input into empty stream" in {
    val input = new StringInputStream("")
    val tokens = Lexer.consume(input).toList
    assertResult(0) { tokens.length }
  }

  it should "parse whitespaces into empty stream" in {
    val input = new StringInputStream("")
    val tokens = Lexer.consume(input).toList
    assertResult(0) { tokens.length }
  }
}
