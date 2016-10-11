package language.AST

import language.AST.ASTImplicits.NodesList

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
  protected val childs: ListBuffer[Node] = new ListBuffer[Node]()

  def accept(visitor: Visitor) = {
    visitor.previsit(this)
    childs.foreach(visitor.visit)
    visitor.postvisit(this)
  }

  def isLeaf = childs.isEmpty

  def append(node: Node): Node = {
    childs.append(node)
    this
  }

  def appendAll(childs: Traversable[Node]) = {
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

  def toString: String
}

case class Program() extends Node {
  def expresisons = childs.map { _.asInstanceOf[Expression] }
}

abstract class Expression extends Node { }

case class AssignmentExpression() extends Expression {
  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: AssignmentExpression => true
      case _ => false
    }
  }
  override def toString = "AssignmentExpression"

  def variable = childs.head.asInstanceOf[Word].value

  def value = childs(1).asInstanceOf[Word].value
}

case class PipeExpression() extends Expression {
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
  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: CommandExpression => this.name == that.name
      case _ => false
    }
  }

  def getArgs : ListBuffer[String] = {
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
  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: Word => this.value == that.value
      case _ => false
    }
  }
}







