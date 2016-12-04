package environment

import scala.collection.mutable

/**
  * Holds values of all defined variables.
  *
  * Supports map-like syntax, e.g. `ctx(foo) = "bar"`
  *
  */
class Context(private final val varsToValues: mutable.Map[String, String] = mutable.Map.empty)
    extends mutable.Cloneable[Context] {
  def update(name: String, definition: String): Unit = {
    varsToValues(name) = definition
  }

  def apply(name: String): String = varsToValues(name)

  def getOrElse(name: String, default: String): String = varsToValues.getOrElse(name, default)
  override def clone(): Context = {
    new Context(varsToValues.clone())
  }
}
