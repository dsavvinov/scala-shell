package interpretation.commands

import exceptions.ExternalCommandErrorException
import io.streams.{InputStream, OutputStream, StdInStream, StdOutStream}
import io.{OutputStream, StdInStream, StdOutStream}

import scala.sys.process.{Process, ProcessLogger}

class ExternalCommand private[commands] (
                       override val name: String
                     , override val args: List[String]
                     , override val inputStream: InputStream[String] = StdInStream
                     , override val outputStream: OutputStream[String] = StdOutStream
                     ) extends Command {
  override def run(): Int = {
    var returnCode = 0

    val errors = StringBuilder.newBuilder
    val processLogger = ProcessLogger(
      (out: String) => {
        outputStream.write(out + "\n")
      },
      (err: String) => errors append err
    )

    val fullCmd = name + " " + args.mkString(" ")
    if (inputStream == StdInStream) {
      try {
        returnCode = Process(fullCmd) ! processLogger
      } catch {
        case e : Exception =>
          returnCode = 1
          throw ExternalCommandErrorException(message = e.getMessage, stderr = errors.mkString)
      }
    } else {
      returnCode = Process(fullCmd).#<(inputStream.toJavaInputStream) ! processLogger
    }

    returnCode
  }
}
