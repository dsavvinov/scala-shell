package interpretation.commands

import cli.CLI
import io.streams.{InputStream, OutputStream, StdInStream, StdOutStream}
import io.{OutputStream, StdInStream, StdOutStream}

/**
  * Name
  * exit - exits shell
  *
  * Synopsis
  * exit
  *
  * Description
  * Terminates current shell process.
  */
class Exit private[commands] (
            override val args: List[String]
          , override val inputStream: InputStream[String] = StdInStream
          , override val outputStream: OutputStream[String] = StdOutStream
          ) extends Command {
  override val name: String = "exit"

  override def run(): Int = {
    CLI.shouldStop = true
    0
  }
}
