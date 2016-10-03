package language.AST

abstract class Visitor {
  def visit(node: Node): Any = { }
  def previsit(node: Node): Any = { }
  def postvisit(node: Node): Any = { }
}