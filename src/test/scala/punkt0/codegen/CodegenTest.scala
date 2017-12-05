package punkt0.codegen

import java.io.{ByteArrayOutputStream, File, PrintWriter}

import sys.process.{Process, ProcessBuilder, ProcessIO}
import org.scalatest._
import punkt0.Context
import punkt0.lexer.Lexer
import punkt0.Reporter
import punkt0.analyzer.Symbols.GlobalScope
import punkt0.analyzer.{NameAnalysis, TypeChecking}
import punkt0.ast.Parser
import punkt0.code.CodeGeneration

class CodegenTest extends FlatSpec with Matchers {
  "All source files" should "generate valid bytecode" in {
    val d = new File("./testprograms/lab3/valid")
    val files = d.listFiles.filter(_.isFile).toList
    files.filter(x => !x.getName.contains(".ast")).foreach(f => {
      // Generate the classfiles
      val folder = new File(s"./classfiles/${f.getName}")
      val ctx = Context(
        outDir = Some(folder),
        file = Some(f)
      )
      val lex = Lexer.run(f)(ctx)
      Reporter.terminateIfErrors()
      val parsed = Parser.run(lex)(ctx)
      Reporter.terminateIfErrors()
      val na = NameAnalysis.run(parsed)(ctx)
      Reporter.terminateIfErrors()
      val tc = TypeChecking.run(na)(ctx)
      Reporter.terminateIfErrors()
      CodeGeneration.run(tc)(ctx)
      Reporter.terminateIfErrors()

      // Run the classfiles
      println(s"Program ${folder.getName}")
      runCommand("java Main", folder)

      NameAnalysis.globalScope = new GlobalScope()
    })
  }

  def runCommand(cmd: String, dir: File): Unit = {
    val pb: ProcessBuilder = Process(cmd, dir)

    val proc = pb.run(new ProcessIO(
      _.close(), // stdin
      _.close(),
      _.close()
//      out => { // stdout
//        val src = scala.io.Source.fromInputStream(out)
//        for (line <- src.getLines()) {
//          println(line)
//        }
//      },
//      out => { // stderr
//        val src = scala.io.Source.fromInputStream(out)
//        for (line <- src.getLines()) {
//          println(line)
//        }
//      },
    ))

    val exitCode = proc.exitValue()

    println(s"EXIT: $exitCode")
  }
}
