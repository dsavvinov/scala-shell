# Scala-shell

Bash-like shell implemented in Scala.

## Building

In root-directory:

```sh
user: ~/scala-shell$ sbt assembly
```

After that, to launch shell:

```
user: ~/scala-shell$ cd target/scala-2.11/
user: ~/scala-shell/target/scala-2.11$ scala scalashell-assembly-1.0.jar
```

## Usage

### Commands

Currently, following commands are supported natively:

  - cat
  - echo
  - pwd
  - wc
  - exit

If the command is not recognized by the shell, then it is treated as an external command and resolved by simply calling it in system shell environment.

### Features

  - pipes (`cat file1 file2 | wc`)
  - variables (`foo = "bar"`, `echo $foo` => `bar`)
  - weak quoting (in a double quotation marks)
  - strong quoting (in a single quotation marks)
  - symbol escaping (via backslash)

### Distinctions from Bash

  - Whitespaces are ignored completely (except for quotations content, of course), so `foo = bar`, `foo= bar` and `foo=bar` are all equivalent and valid expressions
  - Pipes **are not lazy**
  - Some commands (like `cat` or `wc`) when called without arguments enter interactive-mode, repeatedly prompting user for another line of input and processing it. In bash, one can exit that mode using CTRL-D (EOF) combination. **Don't use it, this will cause shell to shut down**. Instead, use `\quit`-keyword.
