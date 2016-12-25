package language.AST

import language.AST.visitors.Printer

import scala.collection.mutable.ListBuffer

/**
  * Class that describes Abstract Syntax Tree of the program.
  *
  * Essentially, Syntax Tree is just a labeled Node along with
  * some utility methods for convenient building, printing and
  * comparing trees.
  */
class SyntaxTree(val root: Node) {

  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: SyntaxTree => this.root compareSubtrees that.root
    }
  }

  override def toString: String = {
    val printer = new Printer()
    root accept printer
    printer.mkString
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