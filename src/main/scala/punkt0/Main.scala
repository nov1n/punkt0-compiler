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
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)

    // Run lexer phase if user specified a file
    ctx.file match {
      case Some(f) =>
        val lex = Lexer.run(f)(ctx)
        while(lex.hasNext) println(lex.next())
      case None =>
        println("punkt0c: fatal error: no input files.")
        println(ctx)
        sys.exit(1)
    }
  }
}
