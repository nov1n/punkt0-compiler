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
  val generateReferenceFiles = true
  "All source files" should "generate valid bytecode" in {
    val d = new File("./testprograms/lab3/valid")
    val files = d.listFiles.filter(_.isFile).toList
    files.filter(x => {
        x.getName.endsWith(".p0") &&
        !x.getName.contains("Life")
    }).foreach(f => {
      // Generate the classfiles
      val dir = new File(s"./classfiles/${f.getName}")
      val ctx = Context(
        outDir = Some(dir),
        file = Some(f)
      )
      print(s"Compiling ${f.getName}... ")
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

      // Create reference bytecodes with scalac
      if(generateReferenceFiles) {
        runCommand("mkdir reference", dir)
        var (stdout, stderr, int) = runCommand(s"/usr/local/lib/scala-2.12.4/bin/scalac -d reference ${f.getAbsoluteFile}", dir)
        int should equal (0)
        println("Success.")
      }

      // Run our compiler and scala, compare results
      print(s"Comparing output with scala compiler... ")
      val (punktStdout, punktStderr, punktCode) = runCommand("java Main", dir)
      val (scalaStdout, scalaStderr, scalaCode) = runCommand("/usr/local/lib/scala-2.12.4/bin/scala Main", new File(s"$dir/reference"))
      punktCode should equal (scalaCode)
      punktStdout should equal (scalaStdout)
      punktStderr should equal (scalaStderr)
      print("Success.")

      NameAnalysis.globalScope = new GlobalScope()
    })
  }

  def runCommand(cmd: String, dir: File): (String, String, Int) = {
    val pb: ProcessBuilder = Process(cmd, dir)

    val stdout = StringBuilder.newBuilder
    val stderr = StringBuilder.newBuilder
    val proc = pb.run(new ProcessIO(
      _.close(), // stdin
      out => { // stdout
        val src = scala.io.Source.fromInputStream(out)
        for (line <- src.getLines()) {
          stdout.append(line + "\n")
        }
      },
      out => { // stderr
        val src = scala.io.Source.fromInputStream(out)
        src.mkString
        for (line <- src.getLines()) {
          stderr.append(line + "\n")
        }
      },
    ))

    val exitCode = proc.exitValue()

    (stdout.toString, stderr.toString(), exitCode)
  }
}
