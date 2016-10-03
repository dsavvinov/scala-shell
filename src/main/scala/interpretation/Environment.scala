package interpretation

import scala.collection.{immutable, mutable}

/**
  * Created by dsavv on 19.09.2016.
  */
class Environment {
  val varsToValues: mutable.Map[String, String] = mutable.Map.empty

  def update(name: String, definition: String): Unit = {
    varsToValues(name) = definition
  }

  def apply(name: String): String = varsToValues(name)
}
