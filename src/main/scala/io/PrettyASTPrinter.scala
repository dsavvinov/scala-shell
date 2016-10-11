package io

import language.AST.{Node, Visitor}

import scala.collection.mutable

class PrettyASTPrinter extends Visitor {
  val sb: StringBuilder = mutable.StringBuilder.newBuilder
  var indentation = ""
  val indentationStep = "  "

  def println(text: String) = {
    sb append indentation
    sb append text
    sb append "\n"
  }

  def print(text: String) = {
    sb append indentation
    sb append text
  }

  override def visit(node: Node): Any = {
    print(s"${node.toString}")
    if (!node.isLeaf) {
      sb.append(" {")
      indent()
    }
    sb append "\n"
  }

  override def postvisit(node: Node) = {
    if (!node.isLeaf) {
      outdent()
      println("}")
    }
  }

  def indent() = indentation = indentation + indentationStep
  def outdent() = indentation = indentation.stripSuffix(indentationStep)
}