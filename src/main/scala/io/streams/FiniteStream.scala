package io.streams

trait FiniteStream[T] extends InputStream[T] {
  def toList: List[T]
  def isAtEnd(): Boolean
}
