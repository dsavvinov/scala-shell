package language

import interpretation.Environment
import language.AST.WeakQuotationNode
import org.scalatest.FlatSpec

/**
  * Created by dsavv on 22.09.2016.
  */
class QuotationsTest extends FlatSpec {
  "Weak quotation" should "expand one substitution of one-letter var to one word" in {
    val env = new Environment()
    env("x") = """world!"""

    val quote = WeakQuotationNode("""Hello, ${x}""")
    val result = quote expandUsing env

    assert(result == "Hello, world!")
  }

  it should "expand one substitution of word into one word" in {
    val env = new Environment()
    env("long_word") = """world!"""

    val quote = WeakQuotationNode("""Hello, ${long_word}""")
    val result = quote expandUsing env

    assert(result == "Hello, world!")
  }

  it should "expand one substitution of word into several words" in {
    val env = new Environment()
    env("long_var_name") = """perfect world of Scala parsers and lexers!"""

    val quote = WeakQuotationNode("""Hello, ${long_var_name}""")
    val result = quote expandUsing env

    assert(result == "Hello, perfect world of Scala parsers and lexers!")
  }

  it should "expand multiple substitutions of various kinds" in {
    val env = new Environment()
    env("var1") = """ To parse, or"""
    env("var2") = """ that is the question"""
    env("v") = """ not to parse"""

    val quote = WeakQuotationNode("""${var1}${v},${var2}""")
    val result = quote expandUsing env

    assert(result == " To parse, or not to parse, that is the question")
  }

  it should "expand substitutions with strange characters" in {
    val env = new Environment()
    env("signs_and_stuff") = """!@#$%^&*()_+,./<>?\|}{[]":;'"""

    val quote = WeakQuotationNode("""${signs_and_stuff}""")
    val result = quote expandUsing env

    assert(result == """!@#$%^&*()_+,./<>?\|}{[]":;'""")
  }
}
