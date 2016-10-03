package interpretation

import scala.collection.mutable

sealed trait Command {
  val input: List[String]
  val name: String
  def run(): String
}

/**
  * Name
  * cat - concatenate files and print on the standard output
  *
  * Synopsis
  * cat [OPTION]... [FILE]...
  *
  * Description
  * Concatenate FILE(s), or standard input, to standard output.
  */
class Cat(override val input: List[String]) extends Command {
  override val name: String = "cat"

  override def run(): String = {
    //TODO: read file
    ???
  }
}

/**
  * Name
  * echo - display a line of text
  *
  * Synopsis
  * echo [SHORT-OPTION]... [STRING]...
  * echo LONG-OPTION
  *
  * Description
  * Echo the STRING(s) to standard output.
  */
class Echo (override val input: List[String]) extends Command {
  override val name: String = "echo"

  override def run(): String = input.mkString("\n")
}

/**
  * Name
  * wc - print newline, word, and byte counts for each file
  *
  * Synopsis
  * wc [OPTION]... [FILE]...
  *
  * Description
  * Print newline, word, and byte counts for each FILE, and a total line if more than one FILE is specified.
  * With no FILE, read standard input.
  */
class Wc (override val input: List[String]) extends Command {
  override val name: String = "wc"

  override def run(): String = {
    val lines = input.map { it => it.count ( _ == '\n') }
    val words = input.map { it => it.split(" ").length }

    val result: mutable.StringBuilder = mutable.StringBuilder.newBuilder
    (lines, words).zipped.foreach {
      (lineCount, wordCount) =>
        result.append(s"lines: $lineCount\n")
        result.append(s"words: $wordCount\n")
        result.append("\n")
    }
    result.toString()
  }
}

/**
  * Name
  * pwd - print name of current/working directory
  *
  * Synopsis
  * pwd [OPTION]...
  *
  * Description
  * Print the full filename of the current working directory.
  */
class Pwd(override val input: List[String]) extends Command {
  override val name: String = "wc"
  override def run(): String = {
    //TODO: output current path
    ???
  }
}

class Exit(override val input: List[String]) extends Command {
  override val name: String = "exit"

  override def run(): String = {
    //TODO: exit shell
    ???
  }
}

