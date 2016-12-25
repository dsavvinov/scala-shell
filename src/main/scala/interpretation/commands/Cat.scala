package interpretation.commands

import java.nio.file.{Path, Paths}

import io.streams.{InputStream, OutputStream, StdInStream, StdOutStream}
import io.{OutputStream, StdInStream, StdOutStream}

/**
  * Name
  * cat - concatenate files and print on the standard output
  *
  * Synopsis
  * cat [OPTION]... [FILE]...
  *
  * Description
  * Concatenate FILE(s) to standard output.
  * If the file is not specified, then concatenates standard input.
  */
class Cat private[commands] (
           override val args: List[String]
         , override val inputStream: InputStream[String] = StdInStream
         , override val outputStream: OutputStream[String] = StdOutStream
         ) extends Command {
  override val name: String = "cat"

  override def run(): Int = {
    if (args.isEmpty) {
      // "Interactive" mode
      println("Entering interactive mode. Please, use \"\\quit\" instead of CTRL-D to exit mode!")
      var str = inputStream.read()
      while (str.isDefined && str.get != "\\quit") {
        outputStream.write(str.get + "\n")
        str = inputStream.read()
      }
      return 0
    }

    val currentDir = Paths.get(System.getProperty("user.dir"))
    outputStream.write(
      args.map { (it: String) => currentDir.resolve(it) } // map to absolute paths
        // map each path to String of its contents
        .map { (it: Path) => scala.io.Source.fromFile(it.toFile).mkString }
        // Concatenate all input paths
        .mkString
    )
    0
  }
}
