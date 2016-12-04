package language.AST

import io.PrettyASTPrinter

import scala.collection.mutable.ListBuffer

/**
  * Class that describes Abstract Syntax Tree of the program.
  *
  * Essentially, Syntax Tree is just a labeled Node along with
  * some utility methods for convenient building, printing and
  * comparing trees.
  */
class SyntaxTree(val root: Node) {
  var curNode: Node = root

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

/**
  * Import those implicits to allow fluent DSL-driven
  * construction of SyntaxTrees (see tests for examples)
  */
object ASTImplicits {
  implicit class NodesList(node: Node) {
    val list: ListBuffer[Node] = ListBuffer(node)

    def \(node: Node): NodesList = {
      list append node
      this
    }
  }
}