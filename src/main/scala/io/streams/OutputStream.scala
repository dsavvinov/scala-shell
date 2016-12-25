package io.streams

trait OutputStream[T] {
  def write(value: T)
  def toJavaOutputStream : java.io.OutputStream
}
