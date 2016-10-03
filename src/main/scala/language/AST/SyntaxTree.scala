package language.AST

import io.PrettyASTPrinter

import scala.collection.mutable.ListBuffer

class SyntaxTree(val root: Node) {
  var curNode: Node = root

  def append(tree: SyntaxTree) {}

  def accept(visitor: Visitor) = root accept visitor

  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: SyntaxTree => this.root compareSubtrees that.root
    }
  }

  override def toString: String = {
    val pp = new PrettyASTPrinter()
    root accept pp
    pp.sb.toString()
  }
}

object ASTImplicits {
  implicit class NodesList(node: Node) {
    val list: ListBuffer[Node] = ListBuffer(node)

    def \(node: Node): NodesList = {
      list append node
      this
    }
  }
}



//case class StrongQuotation(text: String) extends Literal(text)
//
//case class WeakQuotation(text: String) extends Literal(text) {
//  def expand
//}