package io.streams

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Implementation of Resettable Input/Output Stream on ListBuffer.
  * Could be used as pipe between two sides (visible as output stream
  * to the one side and as an input stream to the other side)
  */
class ListBufferStream[T](private val buffer: ListBuffer[T] = new ListBuffer[T]())
  extends OutputStream[T]
    with ResettableStream[T]
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
