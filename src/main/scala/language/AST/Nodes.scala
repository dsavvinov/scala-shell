package language.AST

import language.AST.ASTImplicits.NodesList
import language.AST.visitors.Visitor

import scala.collection.mutable.ListBuffer

/**
  * Abstract class of Syntax Tree node.
  *
  * This class demands from all successors to use
  * list of nodes as the method of storing childs.
  * Successors are encouraged to expose additional
  * convenience methods for accessing childrens.
  *
  * Also Node defines some utility methods for
  * comparing and building nodes and trees.
  *
  * It is strongly recommended for all of successors
  * to implement equals() and toString() methods correctly,
  * because some utility services (like pretty-printing or
  * comparing SyntaxTrees) relies on that methods.
  *
  */
abstract class Node {
  val childs: ListBuffer[Node] = new ListBuffer[Node]()

  def accept[T](visitor: Visitor[T]): T = {
    visitor.visit(this)
  }

  def isLeaf: Boolean = childs.isEmpty

  def append(node: Node): Node = {
    childs.append(node)
    this
  }

  def appendAll(childs: Traversable[Node]): Unit = {
    childs.foreach { append }
  }

  // For DSL-driven construction of trees
  def apply(nodesList: NodesList): Node = {
    nodesList.list foreach(it => append(it))
    this
  }

  def compareSubtrees(other: Node): Boolean = {
    if (childs.length != other.childs.length) {
      return false
    }

    (childs, other.childs).zipped.foreach(
      (thisChild, otherChild) => {
        if (!thisChild.equals(otherChild)) {
          return false
        }
        if (!thisChild.compareSubtrees(otherChild)) {
          return false
        }
      })

    true
  }
}

case class Program() extends Node {
  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this)

  def expresisons: ListBuffer[Expression] = childs.map { _.asInstanceOf[Expression] }
}

abstract class Expression extends Node {
  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this)
}

case class AssignmentExpression() extends Expression {

  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this)

  override def equals(that: scala.Any): Boolean = {
    that match {
      case _: AssignmentExpression => true
      case _ => false
    }
  }
  override def toString = "AssignmentExpression"

  def variable: String = childs.head.asInstanceOf[Word].value

  def value: String = childs(1).asInstanceOf[Word].value
}

case class PipeExpression() extends Expression {

  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this)

  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: PipeExpression => true
      case _ => false
    }
  }
  override def toString: String = "PipeExpression"

  def commands : ListBuffer[CommandExpression] = childs.map { _.asInstanceOf[CommandExpression] }
}

case class CommandExpression(name: String) extends Expression {

  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this)

  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: CommandExpression => this.name == that.name
      case _ => false
    }
  }

  def args : ListBuffer[String] = {
    childs.map { _.asInstanceOf[Word].value }
  }

  override def toString: String = s"CommandExpression"
}

/**
  * Node that stores one single word (continuous sequence of symbols
  * without any separators or special chars, or one string literal)
  *
  *
  * It would be nice to specify it more (like, "CommandName", "CommandArgument"),
  * but unfortunately, we can't, because of the such cases:
  *
  *   > foo=ls  # there is no way to know what exactly is the right-hand side
  *   > $foo    # now we use "ls" in place where command-name is required,
  *             # so we understand (only now!) that "ls" was command-name
  *   > ...ls output here...
  *
  * Or even trickier:
  *
  *   > foo="ls -la"  # though this is a string literal, it still can be used as a command!
  *   > $foo
  *   > ...ls output here...
  *
  * So we are forced to use such a vague node-type as "Word" and let interpreter
  * decide what it really is in runtime.
  *
  */
case class Word(value: String) extends Expression {

  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this)

  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: Word => this.value == that.value
      case _ => false
    }
  }
}







