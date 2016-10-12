package interpretation

import io.{InputStream, OutputStream}


object CommandsFactory {
  /**
    * Creates instance of Command with a given name, arguments and
    * input and output streams.
    *
    * If command name is not found, then ExternalCommand will be created,
    * i.e. interpreter will just try to call something with that name from
    * system shell.
    *
    * Commands will use passed input stream for reading and output stream for writing
    */
  def getByName(
                 cmdName: String
                 , args: List[String]
                 , in: InputStream[String]
                 , out: OutputStream[String]
               ): Command = {
    cmdName match {
      case "cat" => new Cat(args, in, out)
      case "echo" => new Echo(args, in, out)
      case "exit" => new Exit(args, in, out)
      case "pwd" => new Pwd(args, in, out)
      case "wc" => new Wc(args, in, out)
      case "grep" => new Grep(args, in, out)
      case other => new ExternalCommand(other, args, in, out)
    }
  }
}