package io

import java.io._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait InputStream[T] {
  def read(): Option[T]
  def toJavaInputStream: java.io.InputStream
}

trait FiniteStream[T] {
  def toList: List[T]
}

trait OutputStream[T] {
  def write(value: T)
  def toJavaOutputStream : java.io.OutputStream
}

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
trait ResettableStream[T] extends InputStream[T] with FiniteStream[T] {
  def head: Option[T]

  def mark()

  def reset()

  def unmark()

  def isAtEnd(): Boolean

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

/**
  * Implementation of Resettable Input stream on String.
  */
class StringInputStream(val buffer: String)
  extends InputStream[Char]
  with ResettableStream[Char]
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

/**
  * Implementation of Resettable Input/Output Stream on ListBuffer.
  * Could be used as pipe between two sides (visible as output stream
  * to the one side and as an input stream to the other side)
  */
class ListBufferStream[T](val buffer: ListBuffer[T] = new ListBuffer[T]())
  extends InputStream[T]
  with OutputStream[T]
  with ResettableStream[T]
  with FiniteStream[T]
{
  def writeAll(traversableOnce: TraversableOnce[T]) = {
    traversableOnce.foreach(char => write(char))
  }

  private var pos: Int = 0
  private val markedPositions: mutable.Stack[Int] = mutable.Stack()


  override def toJavaInputStream: java.io.InputStream = {
    new ByteArrayInputStream(buffer.map { _.toString }.mkString.getBytes("UTF-8"))
  }

  override def toJavaOutputStream: java.io.OutputStream = {
    new ByteArrayOutputStream()
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
    pos >= buffer.length - 1
  }

  override def write(value: T): Unit = {
    buffer.append(value)
  }
}

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

/**
  * Wrapper of stdout to let pass stdout where OutputStream is expected.
  */
object StdOutStream extends OutputStream[String] {
  override def write(value: String): Unit = print(value)

  override def toJavaOutputStream : java.io.OutputStream = System.out
}