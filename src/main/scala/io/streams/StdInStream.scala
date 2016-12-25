package io.streams

/**
  * Wrapper of stdin to let pass stdin where InputStream is expected.
  */
object StdInStream extends InputStream[String] {
  override def read(): Option[String] = {
    val line = scala.io.StdIn.readLine()
    if (line == null) {
      return None
    }
    Some(line)
  }

  override def toJavaInputStream: java.io.InputStream = System.in
}
