package io.streams

import java.io._

import scala.collection.mutable

/**
  * Implementation of Resettable Input stream on String.
  */
class StringInputStream(private val buffer: String)
  extends ResettableStream[Char]
{

  override def toJavaInputStream: java.io.InputStream = {
    new ByteArrayInputStream(buffer.getBytes("UTF-8"))
  }

  private var pos: Int = 0
  private val markedPositions: mutable.Stack[Int] = mutable.Stack()

  override def head: Option[Char] = {
    if (pos == buffer.length) {
      return None
    }
    Some(buffer(pos))
  }

  override def toList: List[Char] = {
    buffer.toList
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





