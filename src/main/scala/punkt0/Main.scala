package punkt0

import java.io.File

import lexer._
import analyzer.{NameAnalysis, TypeChecking}
import ast.{Parser, Printer, PrinterTree}
import punkt0.code.CodeGeneration
import punkt0.resolver.ForeignResolver


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

      case "--astree" :: args =>
        ctx = ctx.copy(doASTree = true)
        processOption(args)

      case "--print" :: args =>
        ctx = ctx.copy(doPrintMain = true)
        processOption(args)

      case "--symid" :: args =>
        ctx = ctx.copy(doSymbolIds = true)
        processOption(args)

      case "-classPath" :: cp :: args =>
        ctx = ctx.copy(classPath = Some(cp))
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
    println(" --ast         print the ast")
    println(" --astree      print the ast as a tree")
    println(" --symid       print the ast with symbol ids")
    println(" --pretty      pretty print the ast")
    println(" --classPath   add folder to classpath")
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
    }
    Reporter.terminateIfErrors()
    if(ctx.doTokens) sys.exit(0)

    // Start parsing using lexer iterator
    val parsed = Parser.run(lex)(ctx)

    // Print and exit if needed
    if(ctx.doASTree) {
      val tree = PrinterTree.apply(parsed)
      print(tree)
      sys.exit(0)
    } else if(ctx.doPrintMain) {
      val pretty = Printer.apply(parsed, printSymbols = false)
      print(pretty)
      sys.exit(0)
    } else if(ctx.doAST) {
      val ast = parsed.toString
      println(ast)
      sys.exit(0)
    }

    val resolved = ForeignResolver.run(parsed)(ctx)
    Reporter.terminateIfErrors()

    val named = NameAnalysis.run(resolved)(ctx)
    Reporter.terminateIfErrors()

    val typeCorrect = TypeChecking.run(named)(ctx)
    Reporter.terminateIfErrors()

    if(ctx.doSymbolIds) {
      val namedAST = Printer.apply(typeCorrect, printSymbols = true)
      print(namedAST)
    }

    val codeGen = CodeGeneration.run(typeCorrect)(ctx)
    // Cannot have errors
  }
}


