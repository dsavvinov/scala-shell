package io.streams

import scala.collection.mutable.ListBuffer

/**
  * Resettable streams can mark some positions in stream and then
  * return to those positions.
  *
  * Resettable stream implementations should support recursive marking,
  * meaning that users of that class can mark several positions and then
  * reset to them in LIFO-order.
  *
  * Example:
  *
  *   rs = MyResettableStream[Char]("hello world")
  *   rs.mark()   // now we have mark at the symbol "h"
  *   rs.read()   // read "h"
  *   rs.mark()   // now we have mark at the "h" and mark at the "e"
  *   rs.read()   // read "e"
  *   rs.reset()  // reset to "e"
  *   rs.reset()  // reset to "h"
  *
  * Note: Users of ResettableStreams are responsible for correct marking and
  *       unmarking of positions in stream.
  */
trait ResettableStream[T] extends FiniteStream[T] {
  def head: Option[T]

  def mark()

  def reset()

  def unmark()

  /**
    * Utility method - will read stream contents until the end or
    * until shouldStop predicate evaluates to True.
    */
  def consumeUntil(shouldStop: Option[T] => Boolean): List[T] = {
    val result = new ListBuffer[T]()
    while (!shouldStop(head) && !isAtEnd()) {
      result.append(read().get)
    }
    result.toList
  }
}
