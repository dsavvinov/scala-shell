package interpretation.commands

import java.nio.file.{Path, Paths}

import io.streams.{InputStream, OutputStream, StdInStream, StdOutStream}
import io.{OutputStream, StdInStream, StdOutStream}

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
class Wc private[commands] (
          override val args: List[String]
        , override val inputStream: InputStream[String] = StdInStream
        , override val outputStream: OutputStream[String] = StdOutStream
        ) extends Command {
  override val name: String = "wc"

  override def run(): Int = {
    if (args.isEmpty) {
      if (inputStream == StdInStream) {
        println("Entering interactive mode. Please, use \"\\quit\" instead of CTRL-D to exit mode!")
      }
      // "Interactive" mode
      var str = inputStream.read()
      while (str.isDefined && str.get != "\\quit") {
        outputStream write (count(str.get) + "\n")
        str = inputStream.read()
      }
      return 0
    }

    // Mode: use arguments as paths to files that should be processed
    val currentDir = Paths.get(System.getProperty("user.dir"))
    outputStream.write(
      args.map { (it: String) => currentDir.resolve(it) } // map to absolute paths
      // map each path to String of its contents
      .map { (it: Path) => scala.io.Source.fromFile(it.toFile).mkString }
      // map each file content to result-string
      .map { count }
      // join with newlines
      .mkString("\n")
    )
    0
  }

  private def count(text: String): String = {
    val lines = text.count(_ == '\n')
    val words = text.split("""\W+""").length

    s"""lines = $lines, words = $words"""
  }
}
