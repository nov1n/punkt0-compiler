package punkt0

import java.io.File

import lexer._


object Main {

  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)

      case "--ast" :: args =>
        ctx = ctx.copy(doAST = true)
        processOption(args)

      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(args)


      case List() =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }

    ctx
  }

  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" -d <outdir>   generates class files in the specified directory")
    println(" --tokens      print tokens as parsed by the lexer")
    println(" --ast         pretty print the ast")
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)

    if(ctx.file.isEmpty) {
      Reporter.error("fatal error: no input files.")
      Reporter.terminateIfErrors()
    }

    val f = ctx.file.get
    val lex = Lexer.run(f)(ctx)

    // Print tokens if option is present
    if(ctx.doTokens) {
      while(lex.hasNext) {
        val n = lex.next()
        println(n)
      }
      sys.exit(0)
    }

    // Start parsing using lexer iterator
    while(lex.hasNext) {
      lex.next()
    }
    Reporter.terminateIfErrors()
  }
}
