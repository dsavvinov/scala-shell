package language.AST.visitors

import language.AST._

/**
  * Visitor pattern with generic-return type.
  * Default implementations cast argument to the type of it direct super-class and reinvoke visit()
  */
trait Visitor[T] {
  def visit(node: Node): T
  def visit(programNode: Program): T = visit(programNode.asInstanceOf[Node])

  def visit(expression: Expression): T = visit(expression.asInstanceOf[Node])

  def visit(assignmentExpression: AssignmentExpression): T =
    visit(assignmentExpression.asInstanceOf[Expression])
  def visit(pipeExpression: PipeExpression): T =
    visit(pipeExpression.asInstanceOf[Expression])
  def visit(commandExpression: CommandExpression): T =
    visit(commandExpression.asInstanceOf[Expression])
  def visit(word: Word): T =
    visit(word.asInstanceOf[Expression])
}