package language.AST

import interpretation.Environment
import language.AST.ASTImplicits.NodesList
import language.{Alphabet, ParseException}

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

/**
  * Created by dsavv on 19.09.2016.
  */
abstract class Node {
  val childs: ListBuffer[Node] = new ListBuffer[Node]()

  def accept(visitor: Visitor): Unit = {
    visitor previsit this
    visitor visit this
    childs.foreach(subtree => {
      subtree accept visitor
    })
    visitor postvisit this
  }

  def append(node: Node): Node = {
    childs.append(node)
    this
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

case class Program() extends Node { }

abstract class Expression extends Node { }

case class PipeExpression() extends Expression {
  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: PipeExpression => true
      case _ => false
    }
  }
  override def toString: String = "PipeExpression"
}

case class CommandExpression(name: String) extends Expression {
  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: CommandExpression => this.name == that.name
      case _ => false
    }
  }

  override def toString: String = s"CommandExpression, name = $name"
}

abstract class Argument(val value: String) extends Expression {
  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: Argument => this.value == that.value
      case _ => false
    }
  }

  override def toString: String = s"Literal, value = $value"
}

case class StrongQuotationNode(override val value: String) extends Argument(value) { }

case class WeakQuotationNode(override val value: String) extends Argument(value) {
  case class Substitution(varName: String, begin: Int, end: Int) { }

  val varsToSubsts: mutable.Map[String, Substitution] = mutable.Map.empty

  def expandUsing(env: Environment): String = {
    var result = value
    for ( (varName, subst) <- env.varsToValues) {
      result = result.replaceAllLiterally("${" + varName + "}", subst)
    }

    result.toString
  }


}

case class CommandOption(override val value: String) extends Argument(value) { }