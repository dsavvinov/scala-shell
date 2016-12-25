package interpretation.commands

import io.OutputStream
import io.streams.{InputStream, OutputStream}

/**
  * Abstract trait of Command.
  *
  * All classes that mix-in that trait should:
  *   - read from a given inputStream instead of stdin
  *   - write to a given outputStream instead of stdout
  *   - return error code as the result of run()
  *
  * Note that list of passed command-line arguments
  * *doesn't include* name of the command itself,
  * i.e. args[0] is a first argument.
  *
  */
trait Command {
  val inputStream: InputStream[String]
  val args: List[String]
  val outputStream: OutputStream[String]
  val name: String

  def run(): Int
}