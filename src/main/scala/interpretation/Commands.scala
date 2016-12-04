package interpretation

import java.io.ByteArrayOutputStream
import java.nio.file.{Path, Paths}

import cli.CLI
import exceptions.ExternalCommandErrorException
import io.{InputStream, OutputStream, StdInStream, StdOutStream}

import scala.sys.process.{Process, ProcessIO, ProcessLogger}

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
sealed trait Command {
  val inputStream: InputStream[String]
  val args: List[String]
  val outputStream: OutputStream[String]
  val name: String

  def run(): Int
}

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
class Cat(
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
class Echo(
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
class Wc(
          override val args: List[String]
        , override val inputStream: InputStream[String] = StdInStream
        , override val outputStream: OutputStream[String] = StdOutStream
        ) extends Command {
  override val name: String = "wc"

  override def run(): Int = {
    if (args.isEmpty) {
      println("Entering interactive mode. Please, use \"\\quit\" instead of CTRL-D to exit mode!")
      // "Interactive" mode
      var str = inputStream.read()
      while (str.isDefined && str.get != "\\quit") {
        outputStream write count(str.get)
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
class Pwd(
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
class Exit(
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

class ExternalCommand(
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

