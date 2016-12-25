package interpretation.commands

import io.streams.{InputStream, OutputStream, StdInStream, StdOutStream}
import io.{OutputStream, StdInStream, StdOutStream}

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
class Echo private[commands] (
            override val args: List[String]
          , override val inputStream: InputStream[String] = StdInStream
          , override val outputStream: OutputStream[String] = StdOutStream
          ) extends Command {
  override val name: String = "echo"

  override def run(): Int = {
    outputStream write args.mkString(" ")
    outputStream write "\n"
    0
  }
}
