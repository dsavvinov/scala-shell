package language.AST.visitors
import language.AST._

import scala.collection.mutable

class Printer extends Visitor[Unit] {
  private val sb: StringBuilder = mutable.StringBuilder.newBuilder
  private var indentation = ""
  private val indentationStep = "  "

  private def indent() = {
    indentation = indentation + indentationStep
  }

  private def outdent() = {
    indentation = indentation.stripSuffix(indentationStep)
  }

  def mkString: String = sb.mkString

  override def visit(node: Node): Unit = {
    sb append (indentation + node.toString)
    if (!node.isLeaf) {
      sb append s"$indentation{\n"
      indent()
      node.childs.foreach { e =>
        e.accept(this)
        sb append ",\n"
      }
      outdent()
      sb append s"$indentation}"
    }
  }
}
