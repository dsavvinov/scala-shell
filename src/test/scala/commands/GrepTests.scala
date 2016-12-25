package commands

import java.nio.charset.StandardCharsets
import java.nio.file._

import interpretation.commands.Grep
import io.streams.ListBufferStream

import collection.JavaConverters._
import org.scalatest.{FlatSpec, Outcome}

import scala.collection.mutable
/**
  * Created by dsavvinov on 12.10.16.
  */
class GrepTests extends FlatSpec {
  val testLines: List[String] = List(
    "hello world",
    "heLlo WoRld",
    "HELLO WORLD",
    "helloworld",
    "   hello   world   "
  )

  override protected def withFixture(test: NoArgTest): Outcome = {
    // Prepare file for testing
    val curDir: Path = Paths.get(System.getProperty("user.dir"))
    val testFile: Path = curDir.resolve("test")

    if (Files.exists(testFile)) {
      Files.delete(testFile)
    }

    Files.createFile(testFile)
    Files.write(testFile, testLines.asJava, StandardCharsets.UTF_8)

    val outcome = super.withFixture(test)

    // Clean after test
    Files.deleteIfExists(testFile)

    outcome
  }

  /**
    * Runs grep with supported args, returns
    * indices of lines in testLines which appeared
    * in output.
    */
  def runGrep(args: String*): Set[Int] = {
    val output = new ListBufferStream[String]()
    val cmd = new Grep(args = args.toList, outputStream = output)
    cmd.run()

    val result: mutable.Set[Int] = mutable.Set.empty

    val cleanedOutput: List[String] = output.toList.map { _.stripPrefix("> ").stripSuffix("\n") }

    for ( (l, ind) <- testLines.zipWithIndex) {
      if (cleanedOutput contains l.stripPrefix("> ").stripSuffix("\n")) {
        result.add(ind)
      }
    }

    result.toSet
  }

  "Grep" should "filter trivial substring" in {
    assertResult( Set(0, 3, 4) ) { runGrep("hello", "test") }
  }

  it should "filter substring ignoring case" in {
    assertResult( Set(0, 1, 2, 3, 4) ) { runGrep("-i", "hello", "test")}
  }

  it should "filter substring matching whole words" in {
    assertResult( Set(0, 4) ) { runGrep("-w", "hello", "test")}
  }

  it should "filter substring ignoring case AND matching whole words" in {
    assertResult( Set(0, 1, 2, 4) ) { runGrep("-i", "-w", "hello", "test") }
  }

  it should "find regex" in {
    assertResult( Set(0, 1, 3, 4) ) { runGrep("h.*o", "test") }
  }

  it should "find regex ignoring case" in {
    assertResult( Set(0, 1, 2, 3, 4) ) { runGrep("-i", "h.*o", "test") }
  }

  it should "find regex ignoring case AND matching whole words" in {
    assertResult( Set(0, 1, 2, 4) ) { runGrep ("-i", "-w", "h...o", "test")}
  }

  it should "output extra lines with -A option" in {
    assertResult( Set(0, 1, 4) ) { runGrep ("-w", "-A", "1", "hello", "test")}
  }
}
