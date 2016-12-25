package interpretation

import environment.Context
import interpretation.commands.CommandsFactory
import io._
import io.streams._
import language.AST._

import scala.collection.mutable.ListBuffer

/**
  * Stateless static wrapper of Executor.
  *
  * run() executes given SyntaxTree in a given Context,
  * producing return-code of evaluation.
  *
  * Pipes are executed in a non-lazy manner, i.e.
  * in the expression "foo | bar" second command
  * will be waiting patiently till the first finish.
  *
  * If at least one of the commands in a pipe returns non-zero
  * termination code, then execution of the whole pipe
  * will be interrupted, and that code will be returned.
  *
  */
object Interpreter {
  def run(tree: SyntaxTree, ctx: Context): Int = {
    val executor = new Executor(ctx)
    executor.execute(tree.root)
  }
}

/**
  * Simple recursive tree-walker that will execute
  * given SyntaxTree.
  *
  * Note that each instance of Executor is bound
  * to context, because it's a part of executor's state.
  * This context will be updated if necessary.
  *
  */
class Executor(private final val ctx: Context) {
  // Source of the input. For the first command in a pipe it is stdin,
  // for all the following - output stream of the previous one.
  var pipeStream: InputStream[String] = StdInStream

  def execute(node: Node): Int = {
    node match {
      case program: Program =>
        for (child <- program.expresisons) {
          // Carefully execute commands one by one, failing after first non-successful
          val errorCode = execute(child)
          if (errorCode != 0) {
            return errorCode
          }
        }
        0

      case pipe: PipeExpression =>
        executePipe(pipe)

      case assignment: AssignmentExpression =>
        executeAssignment(assignment)

      case command: CommandExpression =>
        executeCommand(command)
    }
  }

  private def executeCommand(
                              command: CommandExpression
                            , inputStream: InputStream[String] = StdInStream
                            , outputStream: OutputStream[String] = StdOutStream
                            ): Int = {
    val args: ListBuffer[String] = command.args
    val cmd = CommandsFactory.getByName(command.name, args.toList, inputStream, outputStream)
    cmd.run()
  }

  private def executeAssignment(assignment: AssignmentExpression): Int = {
    ctx.update(assignment.variable, assignment.value)
    0
  }

  private def executePipe(pipe: PipeExpression): Int = {
    for ((command, ind) <- pipe.commands.view.zipWithIndex) {

      // Last command in pipe writes to stdout
      if (ind == pipe.commands.length - 1) {
        val errCode = executeCommand(command, pipeStream, StdOutStream)

        // Don't forget to check error code
        if (errCode != 0) {
          return errCode
        }
      }
      // All other write to tmpStream that will become input for the next command
      else {
        val outputForCur = new ListBufferStream[String]()
        val errCode = executeCommand(command, pipeStream, outputForCur)
        pipeStream = outputForCur

        // Don't forget to check error code
        if (errCode != 0) {
          return errCode
        }
      }
    }
    0
  }

}
