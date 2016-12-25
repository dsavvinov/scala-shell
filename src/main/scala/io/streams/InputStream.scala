package io.streams

trait InputStream[T] {
  def read(): Option[T]
  def toJavaInputStream: java.io.InputStream
}
