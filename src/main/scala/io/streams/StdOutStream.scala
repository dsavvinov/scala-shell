package io.streams

/**
  * Wrapper of stdout to let pass stdout where OutputStream is expected.
  */
object StdOutStream extends OutputStream[String] {
  override def write(value: String): Unit = print(value)

  override def toJavaOutputStream : java.io.OutputStream = System.out
}
