package interpretation.commands

import java.nio.file.Paths

import io.streams.{InputStream, OutputStream, StdInStream, StdOutStream}
import io.{OutputStream, StdInStream, StdOutStream}

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
class Pwd private[commands] (
           override val args: List[String]
         , override val inputStream: InputStream[String] = StdInStream
         , override val outputStream: OutputStream[String] = StdOutStream
         ) extends Command {
  override val name: String = "wc"

  override def run(): Int = {
    outputStream write Paths.get(System.getProperty("user.dir")).toString
    0
  }
}
