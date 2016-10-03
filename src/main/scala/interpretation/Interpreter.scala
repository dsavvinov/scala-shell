package interpretation

import language.AST._

object Interpreter {
  val environment = new Environment()
  def run(tree: SyntaxTree): Option[String] = {
    val executor = new Executor(environment)
    ???
  }
}

class Executor(val environment: Environment) {
  def execute(node: Node): Option[String] = {
    node match {
      case program: Program => runProgram(program)
      case pipe: PipeExpression => runPipe(pipe)
      case cmd: CommandExpression => runCommand(cmd)
      case literal: Argument => processLiteral(literal)
    }
  }

  def processLiteral(literal: Argument): Option[String] = ???

  def runPipe(pipeExpression: PipeExpression) = ???

  def runProgram(program: Program) = ???

  def runCommand(cmd: CommandExpression): Option[String] = {
    // Build argument list, carefully checking that each argument is literal
    val args: List[String] = cmd.childs.map {
      case literal: Argument => literal.value
      case other => throw InvalidArgumentTypeException(
        s"For node $cmd expected Literal as argument, but got $other"
      )
    }.toList

    val result = cmd.name match {
      case "cat" => new Cat(args).run()
      case "echo" => new Echo(args).run()
      case "wc" => new Wc(args).run()
      case "pwd" => new Pwd(args).run()
      case "exit" => new Exit(args).run()
      case other => throw InvalidArgumentTypeException(
        s"Unexpected command name while processing node $cmd"
      )
    }
    ???
  }
}
