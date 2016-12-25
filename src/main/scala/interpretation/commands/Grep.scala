package interpretation.commands

import java.nio.file.{Path, Paths}

import io.streams.{InputStream, OutputStream, StdInStream, StdOutStream}
import io.{OutputStream, StdInStream, StdOutStream}
import org.rogach.scallop.ScallopConf

import scala.collection.mutable.ListBuffer

/**
  * Name
  * grep - print lines matching a pattern
  *
  * Synopsis
  * grep [OPTIONS] PATTERN [FILE...]
  *
  * Description
  *
  * grep searches the named input FILEs (or standard input if no files are named).
  * By default, grep prints the matching lines.
  *
  */
class Grep(
              override val args: List[String]
            , override val inputStream: InputStream[String] = StdInStream
            , override val outputStream: OutputStream[String] = StdOutStream
          ) extends Command {

  override val name: String = "grep"

  class Configuration(arguments: Seq[String]) extends ScallopConf(arguments) {
    val ignoreCase = opt[Boolean](default = Some(false))
    val wholeWords = opt[Boolean](default = Some(false))
    val after = opt[Int](short = 'A', validate = { (num: Int) => num >= 0}, default = Some(0) )
    val regex = trailArg[String](name = "Regular expression", default = Some(""))
    val files = trailArg[List[String]](name = "File names", default = Some(Nil), required = false)
    verify()
  }

  override def run(): Int = {
    // Prepare search pattern
    val conf = new Configuration(args)

    var modifierPrefix = ""
    var modifierSuffix = ""

    if (conf.ignoreCase()) {
      modifierPrefix = modifierPrefix + "(?i)"
    }

    if (conf.wholeWords()) {
      modifierPrefix = modifierPrefix + "\\b"
      modifierSuffix = modifierSuffix + "\\b"
    }

    val pattern = (modifierPrefix + conf.regex() + modifierSuffix).r

    // Prepare text
    val wholeInput = ListBuffer[String]()

    if (conf.files().isEmpty) {
      // If files are not specified, then use input stream as source
      // Note that input stream doesn't guarantee that all items will
      // be separate lines, so we should care about splitting them

      var curItem = inputStream.read()
      while (curItem.isDefined && curItem.get != "\\quit") {
        wholeInput.appendAll(curItem.get.split("\n"))
        curItem = inputStream.read()
      }
    } else {
      val currentDir = Paths.get(System.getProperty("user.dir"))
      // Read and concat files
      wholeInput appendAll
        conf.files().map { (it: String) => currentDir.resolve(it) } // map to absolute paths
        // map each path to String of its contents
        .map { (it: Path) => scala.io.Source.fromFile(it.toFile).mkString }
        // Concatenate all input paths
        .mkString
        // Split into lines
        .split("\n")
    }

    for ( (line, ind) <- wholeInput.zipWithIndex) {
      // Check if line contains pattern
      if (pattern.findFirstIn(line).nonEmpty) {
        // If contains, then output it and some lines after it,
        // depending on supported -A option
        (ind to (ind + conf.after())).foreach { ind =>
          if (ind < wholeInput.length) {
            outputStream write (wholeInput(ind) + "\n")
          }
        }
      }
    }
    0
  }
}
