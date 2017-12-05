package punkt0.ast
import java.io.{File, FileWriter}

import org.scalatest._
import punkt0.Context
import punkt0.lexer.Lexer
import punkt0.Reporter

class ParserTest extends FlatSpec with Matchers {

  "The AST object" should "produce correct ASTs from source code" in {
    println("--- AST ---")
    val d = new File("./testprograms/lab3/valid")
    val files = d.listFiles.filter(x => {
      x.isFile &&
      x.getName.endsWith(".p0")
    }).toList
    files.filter(x => !x.getName.contains(".ast")).foreach(f => {
      val ctx = Context()
      val lex = Lexer.run(f)(ctx)
      Reporter.terminateIfErrors()

      // Run parser
      val parsed = Parser.run(lex)(ctx).toString.trim
      val expected = scala.io.Source.fromFile("./testprograms/lab3/valid/"+f.getName+".ast").mkString.trim

      if (parsed != expected) {
        println("x - " + f.getName)
      }
      else println("v - " + f.getName)

      parsed should equal (expected)
    })
  }

  it should "fail on incorrect source programs" in {
    val d = new File("./testprograms/lab3/invalid")
    val files = d.listFiles.filter(_.isFile).toList
    files.foreach(f => {
      val ctx = Context()
      val lex = Lexer.run(f)(ctx)

      // Run parser
      //println(Parser.run(lex)(ctx).toString.trim)
    })
  }

  it should "print(parse(P)) = print(parse(print(parse(P))))" in {
    println()
    println("--- Printer ---")
    val d = new File("./testprograms/lab3/valid")
    val files = d.listFiles.filter(_.isFile).toList
    files.filter(x => !x.getName.contains(".ast")).foreach(f => {
      val ctx = Context()
      val lex = Lexer.run(f)(ctx)
      Reporter.terminateIfErrors()

      // Pretty print parsed
      val parsed = Parser.run(lex)(ctx)
      val printParse = Printer.apply(parsed, printSymbols = false)

      // Run parser on pretty printed output
      val tmp = File.createTempFile(f.getName, ".pp")
      val fw = new FileWriter(tmp)
      fw.write(printParse)
      fw.close()
      val ctx2 = Context()
      val lex2 = Lexer.run(tmp)(ctx2)
      val printParsePrintParse = Printer.apply(Parser.run(lex2)(ctx), printSymbols = false)

      if (printParse != printParsePrintParse) {
        println("x - " + f.getName)
      }
      else {
        println("v - " + f.getName)
        tmp.delete()
      }

      printParse should equal (printParsePrintParse)
    })
  }
}
