package io

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait InputStream[T] {
  def read(): Option[T]
  def head: Option[T]
  def mark()
  def reset()
  def unmark()
  def unread()
  def isAtEnd(): Boolean
  def toList: List[T]

  def consumeUntil(shouldStop: Option[T] => Boolean): List[T] = {
    val result = new ListBuffer[T]()
    while (!shouldStop(head) && !isAtEnd()) {
      result.append(read().get)
    }
    result.toList
  }
}

trait OutputStream[T] {
  def write(value: T)
  def toList: List[T]
}

class StringInputStream(val buffer: String) extends InputStream[Char] {
  private var pos: Int = 0
  private val markedPositions: mutable.Stack[Int] = mutable.Stack()

  override def head: Option[Char] = {
    if (pos == buffer.length) {
      return None
    }
    Some(buffer(pos))
  }

  override def toList: List[Char] = {
    return buffer.toList
  }

  override def unread(): Unit = {
    pos -= 1
  }

  override def mark(): Unit = {
    markedPositions.push(pos)
  }

  override def reset(): Unit = {
    pos = markedPositions.pop()
  }

  override def unmark(): Unit = {
    markedPositions.pop()
  }

  override def read(): Option[Char] = {
    if (pos == buffer.length) {
      return None
    }

    pos += 1
    Some(buffer.charAt(pos - 1))
  }

  override def isAtEnd(): Boolean = {
    pos == buffer.length
  }
}

class ListBufferStream[T] (val buffer: ListBuffer[T] = new ListBuffer[T]())
  extends InputStream[T]
    with OutputStream[T]
{
  private var pos: Int = 0
  private val markedPositions: mutable.Stack[Int] = mutable.Stack()

  override def unread(): Unit = {
    if (pos != 0) {
      pos -= 1
    }
  }

  override def toList: List[T] = buffer.toList

  override def mark(): Unit = {
    markedPositions.push(pos)
  }

  override def reset(): Unit = {
    pos = markedPositions.pop()
  }


  override def unmark(): Unit = markedPositions.pop()

  override def read(): Option[T] = {
    if (pos == buffer.length) {
      return None
    }

    pos += 1
    Some(buffer(pos - 1))
  }


  override def head: Option[T] = {
    if (pos == buffer.length) {
      return None
    }
    Some(buffer(pos))
  }

  override def isAtEnd(): Boolean = {
    pos == buffer.length - 1
  }

  override def write(value: T): Unit = {
    buffer.append(value)
  }
}