package language

import environment.Context
import io.streams.StringInputStream
import language.preprocessing.Preprocessor
import org.scalatest.FlatSpec

class QuotationsTest extends FlatSpec {
  "Weak quotation" should "expand one substitution of one-letter var to one word" in {
    val env = new Context()
    env("x") = """world!"""

    val quote = """Hello, ${x}"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == "Hello, world!")
  }

  it should "expand one substitution of word into one word" in {
    val env = new Context()
    env("long_word") = """world!"""

    val quote = """Hello, ${long_word}"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == "Hello, world!")
  }

  it should "expand one substitution of word into several words" in {
    val env = new Context()
    env("long_var_name") = """perfect world of Scala parsers and lexers!"""

    val quote = """Hello, ${long_var_name}"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == "Hello, perfect world of Scala parsers and lexers!")
  }

  it should "expand multiple substitutions of various kinds" in {
    val env = new Context()
    env("var1") = """ To parse, or"""
    env("var2") = """ that is the question"""
    env("v") = """ not to parse"""

    val quote = """${var1}${v},${var2}"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == " To parse, or not to parse, that is the question")
  }

  it should "expand substitutions with strange characters" in {
    val env = new Context()
    env("signs_and_stuff") = """!@#$%^&*()_+,./<>?\|}{[]":;'"""

    val quote = """${signs_and_stuff}"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == """!@#$%^&*()_+,./<>?\|}{[]":;'""")
  }

  it should "expand substitution without curly brackets" in {
    val env = new Context()
    env("x") = """echo"""

    val quote = """$x"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == "echo")
  }

  it should "expand space-separated substitutions w/o curly brackets" in {
    val env = new Context()
    env("xxx") = """ls"""
    env("yyy") = """-la"""

    val quote = """$xxx $yyy"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == "ls -la")
  }

  it should "expand dollar-separated substitutions w/o curly brackets" in {
    val env = new Context()
    env("xxx") = """l"""
    env("yyy") = """s -la"""

    val quote = """$xxx$yyy"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == "ls -la")
  }

  it should "not stop on escaped chars" in {
    val env = new Context()
    env("name1") = "value1"

    val quote = """\$\{name1\}$name1"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == """\$\{name1\}value1""")
  }

  it should "expand absent values into empty strings"
  val env = new Context()

  val quote = """hello, ${foo}world"""
  val result = Preprocessor.process(quote, env).toList.mkString

  assertResult("hello, world") { result }

  "Strong quote" should "not expand anything" in {
    val env = new Context()
    env("x") = """some word"""

    val quote = """it shouldnt '$x' be expanded"""
    val result = Preprocessor.process(quote, env).toList.mkString

    assert(result == """it shouldnt '$x' be expanded""")
  }
}
